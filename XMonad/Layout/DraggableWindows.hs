{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, PatternGuards, TypeSynonymInstances, ConstraintKinds, KindSignatures,
             Rank2Types, StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Draggable
-- Copyright   :  (c) 2013 Patrick Mahoney
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  paddy.mahoney@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier and a class for easily creating draggable
-- layouts.
-- module XMonad.Layout.Decoration
--     ( -- * Usage:
--       -- $usage
--       decoration
--     , Theme (..), defaultTheme
--     , Decoration
--     , DecorationMsg (..)
--     , DecorationStyle (..)
--     , DefaultDecoration (..)
--     , Shrinker (..), DefaultShrinker
--     , shrinkText, CustomShrink ( CustomShrink ), shrinkWhile
--     , isInStack, isVisible, isInvisible, isWithin, fi
--     , findWindowByDecoration
--     , module XMonad.Layout.LayoutModifier
--     , DecorationState, OrigWin
--     ) where
module XMonad.Layout.DraggableWindows
    ( -- * Usage:
      -- $usage
     makeDraggable,
     makeDraggableWindowSwitcher
      
    ) where
      
import Control.Monad (unless)
import Data.Maybe
import Data.List
import Foreign.C.Types(CInt)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), diff, listFromList)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Invisible
import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Util.Image
import XMonad.Layout.DraggingVisualizer

type DraggedWindow a = (a, Rectangle)

data DragWindow a = DragWindow (Invisible Maybe (DraggedWindow a)) deriving (Show, Read, Eq)

initState :: Invisible Maybe (DraggedWindow a)
initState = I Nothing

makeDraggable :: l a -> ModifiedLayout DragWindow l a 
makeDraggable = ModifiedLayout (DragWindow initState)
 
instance (Eq (Invisible Maybe (DraggedWindow a)))

instance (Show a, Read a, Eq a) => (Draggable (DragWindow a))

class (Show a, Read a) => Draggable a where
    describeDraggable :: a -> String
    describeDraggable a = show a 
  
    -- | A hook that can be used to catch the cases when the user
    -- clicks on the window. If you return True here, the click event
    -- will be considered as dealt with and no further processing will take place.
    catchClicks :: a -> CInt -> CInt -> X Bool
    catchClicks _ _ _ = return False
    
    -- | This hook is called while a window is dragged.
    -- The hook can be overwritten if a different way of handling the dragging
    -- is required.
    whileDragging :: a -> CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
    whileDragging _ x y (window, rect) x_pos y_pos = do spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                                        handleDragInProgress x y (window, rect) x_pos y_pos

    -- | This hook is called after a window has been dragged.
    afterDragging :: a -> (Window, Rectangle) -> Window -> X ()
    afterDragging _a (window, rect) _ = do spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                           focus window
    
instance Draggable Window where            
    describeDraggable _ = "WindowSwitcherDragger" 

    catchClicks _ _ _ = return False
      
    whileDragging _ ex ey (mainw, r) x y = do spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                              handleTiledDraggingInProgress ex ey (mainw, r) x y
                                              
    afterDragging _ (mainw, _) decoWin = do focus mainw
                                            sendMessage $ DraggingStopped
                                            spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                            performWindowSwitching mainw   

-- instance (Draggable Window) => LayoutModifier DragWindow Window where 
--     handleMess dw m
--         | Just e <- fromMessage m                = do handleEvent dw e
--                                                       return Nothing
--     handleMess _ _ = return Nothing
-- 
--     modifierDescription dw = describeDraggable dw
    

handleEvent :: (Draggable Window) => DragWindow Window -> Event -> X ()
handleEvent (DragWindow (I (Just dw))) ButtonEvent { ev_window = window
                          , ev_event_type = event_type
                          , ev_x_root = x
                          , ev_y_root = y}
  | event_type == buttonPress = do
      dealtWith <- catchClicks window x y
      unless dealtWith $ do 
        mouseDrag (\ new_x new_y -> focus window >> whileDragging window x y dw new_x new_y)
                    (afterDragging window dw window)
handleEvent _ _ = return ()    
    
handleDragInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleDragInProgress x y (window, rect) x_pos y_pos = 
  do
    let new_rect = Rectangle (x_pos - (fi x - rect_x rect))
                         (y_pos - (fi y - rect_y rect))
                         (rect_width  rect)
                         (rect_height rect)
    sendMessage $ SetGeometry new_rect
   
makeDraggableWindowSwitcher :: l a -> ModifiedLayout WindowSwitcherDraggable l a 
makeDraggableWindowSwitcher = ModifiedLayout (WindowSwitcherDraggable initState)    

data WindowSwitcherDraggable a = WindowSwitcherDraggable (Invisible Maybe (DraggedWindow a)) deriving (Show, Read, Eq)
    
                                           
handleTiledDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleTiledDraggingInProgress ex ey (mainw, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ DraggingWindow mainw rect
    
performWindowSwitching :: Window -> X ()
performWindowSwitching win =
    withDisplay $ \d -> do
       root <- asks theRoot
       (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root
       ws <- gets windowset
       let allWindows = W.index ws
       -- do a little double check to be sure
       if (win `elem` allWindows) && (selWin `elem` allWindows)
            then do
                let allWindowsSwitched = map (switchEntries win selWin) allWindows
                let (ls, t:rs) = break (win ==) allWindowsSwitched
                let newStack = W.Stack t (reverse ls) rs
                windows $ W.modify' $ \_ -> newStack
            else return ()
    where
        switchEntries a b x
            | x == a    = b
            | x == b    = a
            | otherwise = x    
            
instance Draggable (WindowSwitcherDraggable a) where            
    describeDraggable _ = "WindowSwitcherDragger" 

    catchClicks (WindowSwitcherDraggable window) dFL dFR = do spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                                              return False
      
    whileDragging _ ex ey (mainw, r) x y = do spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                              handleTiledDraggingInProgress ex ey (mainw, r) x y
                                              
    afterDragging _ (mainw, _) decoWin = do focus mainw
                                            sendMessage $ DraggingStopped
                                            spawn ("kdialog -msgbox " ++ "DraggingStopped")
                                            performWindowSwitching mainw   

instance (Draggable Window) => LayoutModifier WindowSwitcherDraggable Window where 
    handleMess dw m
        | Just e <- fromMessage m                = do handleEventWS dw e
                                                      return Nothing
    handleMess _ _ = return Nothing

    modifierDescription dw = describeDraggable dw
        
handleEventWS :: (Draggable Window) => WindowSwitcherDraggable Window -> Event -> X ()
handleEventWS (WindowSwitcherDraggable (I (Just (window,rect)))) ButtonEvent { ev_window = this_w
                                                              , ev_event_type = event_type
                                                              , ev_x_root = x
                                                              , ev_y_root = y}
  | event_type == buttonPress = do
      let (Rectangle x_pos y_pos width height) = rect
      dealtWith <- catchClicks window x y
      unless dealtWith $ do 
        mouseDrag (\ new_x new_y -> focus window >> whileDragging window x y (window, rect) new_x new_y)
                    (afterDragging window (window, rect) window)
handleEventWS _ _ = return ()

