// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/jsx.h>
#include <quick-lint-js/linked-bump-allocator.h>
#include <string>
#include <string_view>
#include <unordered_map>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
const std::unordered_map<string8_view, jsx_attribute>& jsx_attribute_aliases() {
  static linked_bump_allocator<1> string_allocator;
  static const std::unordered_map<string8_view, jsx_attribute> cache = [] {
    // FIXME(strager): This is very inefficient.
    std::unordered_map<string8_view, jsx_attribute> aliases;

    // Compatibility with React.js as of January 12, 2022:
    // https://github.com/facebook/react/blob/c09596cc6021e1f9f8a88179add93f80fc07823b/packages/react-dom/src/shared/possibleStandardNames.js
    string8_view attribute_names[] = {
        u8"onClick"sv,
        u8"onMouseEnter"sv,
        // TODO(strager): Add more event handler aliases (onFoo).

        u8"accentHeight"sv,
        u8"acceptCharset"sv,
        u8"accessKey"sv,
        u8"alignmentBaseline"sv,
        u8"allowFullScreen"sv,
        u8"allowReorder"sv,
        u8"arabicForm"sv,
        u8"attributeName"sv,
        u8"attributeType"sv,
        u8"autoCapitalize"sv,
        u8"autoComplete"sv,
        u8"autoCorrect"sv,
        u8"autoFocus"sv,
        u8"autoPlay"sv,
        u8"autoReverse"sv,
        u8"autoSave"sv,
        u8"baseFrequency"sv,
        u8"baseProfile"sv,
        u8"baselineShift"sv,
        u8"calcMode"sv,
        u8"capHeight"sv,
        u8"cellPadding"sv,
        u8"cellSpacing"sv,
        u8"charSet"sv,
        u8"classID"sv,
        u8"className"sv,
        u8"clipPath"sv,
        u8"clipPathUnits"sv,
        u8"clipRule"sv,
        u8"colSpan"sv,
        u8"colorInterpolation"sv,
        u8"colorInterpolationFilters"sv,
        u8"colorProfile"sv,
        u8"colorRendering"sv,
        u8"contentEditable"sv,
        u8"contentScriptType"sv,
        u8"contentStyleType"sv,
        u8"contextMenu"sv,
        u8"controlsList"sv,
        u8"crossOrigin"sv,
        u8"dangerouslySetInnerHTML"sv,
        u8"dateTime"sv,
        u8"defaultChecked"sv,
        u8"defaultValue"sv,
        u8"diffuseConstant"sv,
        u8"disablePictureInPicture"sv,
        u8"disableRemotePlayback"sv,
        u8"dominantBaseline"sv,
        u8"edgeMode"sv,
        u8"enableBackground"sv,
        u8"encType"sv,
        u8"enterKeyHint"sv,
        u8"externalResourcesRequired"sv,
        u8"fillOpacity"sv,
        u8"fillRule"sv,
        u8"filterRes"sv,
        u8"filterUnits"sv,
        u8"floodColor"sv,
        u8"floodOpacity"sv,
        u8"fontFamily"sv,
        u8"fontSize"sv,
        u8"fontSizeAdjust"sv,
        u8"fontStretch"sv,
        u8"fontStyle"sv,
        u8"fontVariant"sv,
        u8"fontWeight"sv,
        u8"formAction"sv,
        u8"formEncType"sv,
        u8"formMethod"sv,
        u8"formNoValidate"sv,
        u8"formTarget"sv,
        u8"frameBorder"sv,
        u8"glyphName"sv,
        u8"glyphOrientationHorizontal"sv,
        u8"glyphOrientationVertical"sv,
        u8"glyphRef"sv,
        u8"gradientTransform"sv,
        u8"gradientUnits"sv,
        u8"horizAdvX"sv,
        u8"horizOriginX"sv,
        u8"hrefLang"sv,
        u8"htmlFor"sv,
        u8"httpEquiv"sv,
        u8"imageRendering"sv,
        u8"imageSizes"sv,
        u8"imageSrcSet"sv,
        u8"innerHTML"sv,
        u8"inputMode"sv,
        u8"itemID"sv,
        u8"itemProp"sv,
        u8"itemRef"sv,
        u8"itemScope"sv,
        u8"itemType"sv,
        u8"kernelMatrix"sv,
        u8"kernelUnitLength"sv,
        u8"keyParams"sv,
        u8"keyPoints"sv,
        u8"keySplines"sv,
        u8"keyTimes"sv,
        u8"keyType"sv,
        u8"lengthAdjust"sv,
        u8"letterSpacing"sv,
        u8"lightingColor"sv,
        u8"limitingConeAngle"sv,
        u8"marginHeight"sv,
        u8"marginWidth"sv,
        u8"markerEnd"sv,
        u8"markerHeight"sv,
        u8"markerMid"sv,
        u8"markerStart"sv,
        u8"markerUnits"sv,
        u8"markerWidth"sv,
        u8"maskContentUnits"sv,
        u8"maskUnits"sv,
        u8"maxLength"sv,
        u8"mediaGroup"sv,
        u8"minLength"sv,
        u8"noModule"sv,
        u8"noValidate"sv,
        u8"numOctaves"sv,
        u8"overlinePosition"sv,
        u8"overlineThickness"sv,
        u8"paintOrder"sv,
        u8"pathLength"sv,
        u8"patternContentUnits"sv,
        u8"patternTransform"sv,
        u8"patternUnits"sv,
        u8"playsInline"sv,
        u8"pointerEvents"sv,
        u8"pointsAtX"sv,
        u8"pointsAtY"sv,
        u8"pointsAtZ"sv,
        u8"preserveAlpha"sv,
        u8"preserveAspectRatio"sv,
        u8"primitiveUnits"sv,
        u8"radioGroup"sv,
        u8"readOnly"sv,
        u8"refX"sv,
        u8"refY"sv,
        u8"referrerPolicy"sv,
        u8"renderingIntent"sv,
        u8"repeatCount"sv,
        u8"repeatDur"sv,
        u8"requiredExtensions"sv,
        u8"requiredFeatures"sv,
        u8"rowSpan"sv,
        u8"shapeRendering"sv,
        u8"specularConstant"sv,
        u8"specularExponent"sv,
        u8"spellCheck"sv,
        u8"spreadMethod"sv,
        u8"srcDoc"sv,
        u8"srcLang"sv,
        u8"srcSet"sv,
        u8"startOffset"sv,
        u8"stdDeviation"sv,
        u8"stitchTiles"sv,
        u8"stopColor"sv,
        u8"stopOpacity"sv,
        u8"strikethroughPosition"sv,
        u8"strikethroughThickness"sv,
        u8"strokeDasharray"sv,
        u8"strokeDashoffset"sv,
        u8"strokeLinecap"sv,
        u8"strokeLinejoin"sv,
        u8"strokeMiterlimit"sv,
        u8"strokeOpacity"sv,
        u8"strokeWidth"sv,
        u8"suppressContentEditableWarning"sv,
        u8"suppressHydrationWarning"sv,
        u8"surfaceScale"sv,
        u8"systemLanguage"sv,
        u8"tabIndex"sv,
        u8"tableValues"sv,
        u8"targetX"sv,
        u8"targetY"sv,
        u8"textAnchor"sv,
        u8"textDecoration"sv,
        u8"textLength"sv,
        u8"textRendering"sv,
        u8"underlinePosition"sv,
        u8"underlineThickness"sv,
        u8"unicodeBidi"sv,
        u8"unicodeRange"sv,
        u8"unitsPerEm"sv,
        u8"useMap"sv,
        u8"vAlphabetic"sv,
        u8"vHanging"sv,
        u8"vIdeographic"sv,
        u8"vMathematical"sv,
        u8"vectorEffect"sv,
        u8"vertAdvY"sv,
        u8"vertOriginX"sv,
        u8"vertOriginY"sv,
        u8"viewBox"sv,
        u8"viewTarget"sv,
        u8"wordSpacing"sv,
        u8"writingMode"sv,
        u8"xChannelSelector"sv,
        u8"xHeight"sv,
        u8"xlinkActuate"sv,
        u8"xlinkArcrole"sv,
        u8"xlinkHref"sv,
        u8"xlinkRole"sv,
        u8"xlinkShow"sv,
        u8"xlinkTitle"sv,
        u8"xlinkType"sv,
        u8"xmlBase"sv,
        u8"xmlLang"sv,
        u8"xmlSpace"sv,
        u8"xmlnsXlink"sv,
        u8"yChannelSelector"sv,
        u8"zoomAndPan"sv,
    };
    for (string8_view attribute_name : attribute_names) {
      char8* lowered_attribute_name =
          string_allocator.allocate_uninitialized_array<char8>(
              attribute_name.size());
      std::transform(attribute_name.begin(), attribute_name.end(),
                     lowered_attribute_name, tolower);
      auto [_it, inserted] = aliases.try_emplace(
          string8_view(lowered_attribute_name, attribute_name.size()),
          jsx_attribute{attribute_name});
      QLJS_ASSERT(inserted);
    }

    return aliases;
  }();
  return cache;
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
