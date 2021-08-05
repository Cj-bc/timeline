{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import GI.Gtk (Box(..)
              , Window(..)
              , Orientation(..)
              , Label(..)
              , Grid(..)
              , Button(..)
              , PositionType(..)
              )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import qualified GI.Gtk.Declarative.Container.Grid as G
import Control.Monad (void)
import Text.ICalendar.Types
import qualified Data.Vector as V
import qualified Data.Text as T

data AppState = AppState { scheduleCal :: VCalendar -- ^ Calendar that holds schedule for today.
                         , logbookCal :: VCalendar -- ^ Calendar that holds clock-in data. The term 'logbok' is comes from Emacs org-mode's LOGBOOK.
                         }

instance Semigroup AppState where
  (AppState s l) <> (AppState s' l') = AppState (s <> s') (l <> l')
instance Monoid AppState where
  mempty = AppState mempty mempty

data AppEvent = AppClosed

timeView :: Widget AppEvent
timeView = container Box [#orientation := OrientationVertical] . fmap timeView' . V.fromList $ timestamp'
  where
    timestamp' :: [T.Text]
    timestamp' = do
      h <- [1..24]
      m <- [":00", ":30"]
      return $ (T.pack . show) h <> m
    timeView' :: T.Text -> BoxChild AppEvent 
    timeView' t = widget Label [#label := t, #vexpand := True, #hexpand := False]

-- | I better to move this to other module at least
calendarTimelineView :: VCalendar -> Widget AppEvent
calendarTimelineView c = container Grid [classes ["cal_1"]] $ V.map construct (V.fromList $ replicate 24 1)
  where
    construct height = G.GridChild { G.properties = G.GridChildPropertiesNextTo height 1 PositionTypeBottom
                                   , G.child = widget Button [#label := "Helllo" , #vexpand := True]
                                   }

  
scheduleView :: AppState -> Widget AppEvent
scheduleView = calendarTimelineView . scheduleCal

logbookView :: AppState -> Widget AppEvent
logbookView = calendarTimelineView . logbookCal

view' :: AppState ->  AppView Window AppEvent 
view' s = bin Window [#name := "timeline"
                     , on #deleteEvent (const (True, AppClosed)) ]
          . container Box [#orientation := OrientationHorizontal]
          $ fmap (BoxChild defaultBoxChildProperties) [ timeView, scheduleView s, logbookView s]

update' :: AppState -> AppEvent -> Transition AppState AppEvent
update' _ AppClosed = Exit
update' s _ = Transition s (pure Nothing)
  
main :: IO ()
main = void $ run App { view = view'
                      , update = update'
                      , inputs = []
                      , initialState = mempty -- TODO: Will read data from files
                      }
