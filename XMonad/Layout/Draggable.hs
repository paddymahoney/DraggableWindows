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
module Draggable2
    ( -- * Usage:
      -- $usage
     makeDraggable
      
    ) where
      
import Control.Monad (when)
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
instance (Show a, Read a, Eq a) => LayoutModifier DragWindow a    

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
    whileDragging _ x y (window, rect) x_pos y_pos = handleDraggingInProgress x y (window, rect) x_pos y_pos

    -- | This hook is called after a window has been dragged.
    afterDragging :: a -> (Window, Rectangle) -> Window -> X ()
    afterDragging _a (window, rect) _ = focus window
    
handleEvent :: (Draggable Window) => DragWindow Window -> Event -> X ()
handleEvent (DragWindow (I (Just (window,rect)))) ButtonEvent { ev_window = this_w
                                                              , ev_event_type = event_type
                                                              , ev_x_root = x
                                                              , ev_y_root = y}
  | event_type == buttonPress = do
      let (Rectangle x_pos y_pos width height) = rect
      dealtWith <- catchClicks window x y
      when (not dealtWith) $ do 
        mouseDrag (\ new_x new_y -> focus window >> whileDragging window x y (window, rect) new_x new_y)
                    (afterDragging window (window, rect) window)
handleEvent _ _ = return ()


handleDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleDraggingInProgress x y (_, rect) x_pos y_pos = 
  do
    let erect = Rectangle (x_pos - (fi x - rect_x rect))
                         (y_pos - (fi y - rect_y rect))
                         (rect_width  rect)
                         (rect_height rect)
    sendMessage $ SetGeometry erect
        

    
    
    
    
    

            
            


