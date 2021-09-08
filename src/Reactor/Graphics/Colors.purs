module Reactor.Graphics.Colors where

import Prelude
import Color (Color, fromHexString)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

fromHex :: String -> Color
fromHex = (unsafePartial fromJust) <<< fromHexString

-- The following colors are a part of the default Tailwind CSS color scheme
-- https://tailwindcss.com/docs/customizing-colors

gray50 :: Color
gray50 = fromHex "#F9FAFB"

gray100 :: Color
gray100 = fromHex "#F3F4F6"

gray200 :: Color
gray200 = fromHex "#E5E7EB"

gray300 :: Color
gray300 = fromHex "#D1D5DB"

gray400 :: Color
gray400 = fromHex "#9CA3AF"

gray500 :: Color
gray500 = fromHex "#6B7280"

gray600 :: Color
gray600 = fromHex "#4B5563"

gray700 :: Color
gray700 = fromHex "#374151"

gray800 :: Color
gray800 = fromHex "#1F2937"

gray900 :: Color
gray900 = fromHex "#111827"

blue50 :: Color
blue50 = fromHex "#EFF6FF"

blue100 :: Color
blue100 = fromHex "#DBEAFE"

blue200 :: Color
blue200 = fromHex "#BFDBFE"

blue300 :: Color
blue300 = fromHex "#93C5FD"

blue400 :: Color
blue400 = fromHex "#60A5FA"

blue500 :: Color
blue500 = fromHex "#3B82F6"

blue600 :: Color
blue600 = fromHex "#2563EB"

blue700 :: Color
blue700 = fromHex "#1D4ED8"

blue800 :: Color
blue800 = fromHex "#1E40AF"

blue900 :: Color
blue900 = fromHex "#1E3A8A"

green50 :: Color
green50 = fromHex "#ECFDF5"

green100 :: Color
green100 = fromHex "#D1FAE5"

green200 :: Color
green200 = fromHex "#A7F3D0"

green300 :: Color
green300 = fromHex "#6EE7B7"

green400 :: Color
green400 = fromHex "#34D399"

green500 :: Color
green500 = fromHex "#10B981"

green600 :: Color
green600 = fromHex "#059669"

green700 :: Color
green700 = fromHex "#047857"

green800 :: Color
green800 = fromHex "#065F46"

green900 :: Color
green900 = fromHex "#064E3B"

red50 :: Color
red50 = fromHex "#FEF2F2"

red100 :: Color
red100 = fromHex "#FEE2E2"

red200 :: Color
red200 = fromHex "#FECACA"

red300 :: Color
red300 = fromHex "#FCA5A5"

red400 :: Color
red400 = fromHex "#F87171"

red500 :: Color
red500 = fromHex "#EF4444"

red600 :: Color
red600 = fromHex "#DC2626"

red700 :: Color
red700 = fromHex "#B91C1C"

red800 :: Color
red800 = fromHex "#991B1B"

red900 :: Color
red900 = fromHex "#7F1D1D"

yellow50 :: Color
yellow50 = fromHex "#FFFBEB"

yellow100 :: Color
yellow100 = fromHex "#FEF3C7"

yellow200 :: Color
yellow200 = fromHex "#FDE68A"

yellow300 :: Color
yellow300 = fromHex "#FCD34D"

yellow400 :: Color
yellow400 = fromHex "#FBBF24"

yellow500 :: Color
yellow500 = fromHex "#F59E0B"

yellow600 :: Color
yellow600 = fromHex "#D97706"

yellow700 :: Color
yellow700 = fromHex "#B45309"

yellow800 :: Color
yellow800 = fromHex "#92400E"

yellow900 :: Color
yellow900 = fromHex "#78350F"

pink50 :: Color
pink50 = fromHex "#FDF2F8"

pink100 :: Color
pink100 = fromHex "#FCE7F3"

pink200 :: Color
pink200 = fromHex "#FBCFE8"

pink300 :: Color
pink300 = fromHex "#F9A8D4"

pink400 :: Color
pink400 = fromHex "#F472B6"

pink500 :: Color
pink500 = fromHex "#EC4899"

pink600 :: Color
pink600 = fromHex "#DB2777"

pink700 :: Color
pink700 = fromHex "#BE185D"

pink800 :: Color
pink800 = fromHex "#9D174D"

pink900 :: Color
pink900 = fromHex "#831843"
