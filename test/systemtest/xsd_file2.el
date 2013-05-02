(require 'genrnc)
(require 'tenv)
(require 'auto-complete-nxml)
(require 'el-expectations)

(expectations
  (desc "genrnc system xsd file 2")
  (expect '("Address" "AgeRecommendation" "AlcoholicBeverages" "AmazonCustomerID" "AmazonOrderID" 
            "AmazonOrderItemCode" "ArtSupplies" "AutoAccessory" "AutoAccessoryMisc" "AutoPart" "BagCase" 
            "Battery" "Beauty" "BeautyMisc" "BedAndBath" "Beverages" "Binocular" "BlankMedia" 
            "BrassAndWoodwindInstruments" "CE" "Camcorder" "CameraPhoto" "CarrierCode" "Cleaner" 
            "Clothing" "ColorMap" "ColorSpecification" "ComputerPlatform" "ConditionType" "ConsumerElectronics" 
            "Darkroom" "DeliveryChannel" "Denomination" "DigitalCamera" "DownloadableFile" "EducationalSupplies" 
            "ExternalCustomerID" "FashionEarring" "FashionNecklaceBraceletAnklet" "FashionOther" "FashionRing" 
            "Film" "FilmCamera" "Filter" "FineEarring" "FineNecklaceBraceletAnklet" "FineOther" "FineRing" "Flash" 
            "Food" "FoodAndBeverages" "FulfillmentCenterID" "FulfillmentMethod" "FulfillmentServiceLevel" 
            "FurnitureAndDecor" "Gourmet" "GourmetMisc" "GraphicsCard" "Guitars" "HandheldSoftwareDownloads" 
            "Health" "HealthMisc" "HeightRecommendation" "Helmet" "Home" "InstrumentPartsAndAccessories" "Jewelry" 
            "KeyboardInstruments" "Kitchen" "Lens" "LensAccessory" "LightMeter" "Lighting" "MarketplaceName" 
            "MerchantFulfillmentID" "MerchantOrderID" "MerchantOrderItemID" "MerchantPromotionID" "Microscope" 
            "MiscWorldInstruments" "Miscellaneous" "MusicalInstruments" "Office" "OfficeProducts" "OtherAccessory" 
            "OutdoorLiving" "PC" "PDA" "PaperProducts" "Pearl" "PercussionInstruments" "PersonalCareAppliances" 
            "PetSupplies" "PetSuppliesMisc" "PhotoPaper" "PhotoStudio" "PowerSupply" "PowersportsPart" 
            "PowersportsVehicle" "Product" "ProductTaxCode" "Projection" "PromotionClaimCode" "ProtectiveGear" 
            "Recall" "RidingApparel" "SKU" "SafetyRating" "SeedsAndPlants" "ShipOption" "ShipmentID" "Software" 
            "SoftwareGames" "SoftwarePlatform" "SoftwareVideoGames" "SoundAndRecordingEquipment" "Sports" 
            "StandardProductID" "StringedInstruments" "SurveillanceSystem" "Telescope" "Tires" "TiresAndWheels" 
            "Tools" "ToysBaby" "TripodStand" "VideoGames" "VideoGamesAccessories" "VideoGamesHardware" "Watch" 
            "WeightRecommendation" "Wheels" "Wireless" "WirelessAccessories" "WirelessDownloads" "WritingInstruments")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir)
           (tfile (tenv-get-tmp-file "genrnc_system" "test.html" t t))
           (cnt 0))
      (genrnc--init-schema-directory t)
      (stub genrnc--read-typeid => "AWS")
      (stub y-or-n-p => t)
      (genrnc-clear-cache)
      (genrnc--log-enable-logging)
      (genrnc--log-clear-log)
      (genrnc-regist-file "aws/Product.xsd")
      (while (and (< cnt 60)
                  (not (with-current-buffer " *log4e-genrnc*"
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward
                            "finish generate '.+aws/Product.xsd'. state:" nil t)))))
        (incf cnt)
        (sleep-for 1))
      (with-current-buffer (find-file-noselect tfile)
        (rng-set-document-type-and-validate "AWS")
        (erase-buffer)
        (goto-char (point-min))
        (insert "<")
        (auto-complete-nxml-get-candidates)))))

