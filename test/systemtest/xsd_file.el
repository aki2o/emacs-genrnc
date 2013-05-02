(require 'genrnc)
(require 'tenv)
(require 'auto-complete-nxml)
(require 'el-expectations)

(expectations
  (desc "genrnc system xsd file")
  (expect '("AddressDetails" "AddressLine" "AdministrativeArea" "Alias" "BalloonStyle" 
            "Camera" "Change" "CountryName" "Create" "Data" "Delete" "Department" "Document" 
            "ExtendedData" "Folder" "GroundOverlay" "Icon" "IconStyle" "ImagePyramid" "ItemIcon" 
            "LabelStyle" "LatLonAltBox" "LatLonBox" "LineString" "LineStyle" "LinearRing" 
            "Link" "ListStyle" "Locality" "Location" "Lod" "LookAt" "Metadata" "Model" 
            "MultiGeometry" "NetworkLink" "NetworkLinkControl" "Orientation" "Pair" "PhotoOverlay" 
            "Placemark" "Point" "PolyStyle" "Polygon" "PostBox" "PostOffice" "PostalCode" "Premise" 
            "PremiseNumber" "PremiseNumberPrefix" "PremiseNumberSuffix" "Region" "ResourceMap" 
            "Scale" "Schema" "SchemaData" "ScreenOverlay" "SimpleData" "SimpleField" "Snippet" 
            "Style" "StyleMap" "Thoroughfare" "ThoroughfareNumber" "ThoroughfareNumberPrefix" 
            "ThoroughfareNumberSuffix" "TimeSpan" "TimeStamp" "Update" "Url" "ViewVolume" "address" 
            "altitude" "altitudeMode" "author" "begin" "bgColor" "bottomFov" "color" "colorMode" 
            "cookie" "coordinates" "description" "displayMode" "displayName" "drawOrder" "east" 
            "email" "end" "expires" "extrude" "fill" "flyToView" "gridOrigin" "heading" "hotSpot" 
            "href" "httpQuery" "innerBoundaryIs" "key" "kml" "latitude" "leftFov" "link" 
            "linkDescription" "linkName" "linkSnippet" "listItemType" "longitude" "maxAltitude" 
            "maxFadeExtent" "maxHeight" "maxLodPixels" "maxSessionLength" "maxSnippetLines" "maxWidth" 
            "message" "minAltitude" "minFadeExtent" "minLodPixels" "minRefreshPeriod" "name" "near" 
            "north" "open" "outerBoundaryIs" "outline" "overlayXY" "phoneNumber" "range" "refreshInterval" 
            "refreshMode" "refreshVisibility" "rightFov" "roll" "rotation" "rotationXY" "scale" 
            "screenXY" "shape" "size" "snippet" "sourceHref" "south" "state" "styleUrl" "targetHref" 
            "tessellate" "text" "textColor" "tileSize" "tilt" "topFov" "uri" "value" "viewBoundScale" 
            "viewFormat" "viewRefreshMode" "viewRefreshTime" "visibility" "west" "when" "width" "x" "xAL" "y" "z")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir)
           (tfile (tenv-get-tmp-file "genrnc_system" "test.html" t t))
           (cnt 0))
      (genrnc--init-schema-directory t)
      (stub genrnc--read-typeid => "KML")
      (stub y-or-n-p => t)
      (genrnc-clear-cache)
      (genrnc--log-enable-logging)
      (genrnc--log-clear-log)
      (genrnc-regist-file "kml/ogckml22.xsd")
      (while (and (< cnt 60)
                  (not (with-current-buffer " *log4e-genrnc*"
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward
                            "finish generate '.+kml/ogckml22.xsd'. state:" nil t)))))
        (incf cnt)
        (sleep-for 1))
      (with-current-buffer (find-file-noselect tfile)
        (rng-set-document-type-and-validate "KML")
        (erase-buffer)
        (goto-char (point-min))
        (insert "<")
        (auto-complete-nxml-get-candidates)))))

