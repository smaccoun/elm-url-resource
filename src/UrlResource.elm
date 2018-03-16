module Routes exposing (..)

import UrlParser as Url exposing ((</>), (<?>), int, s, stringParam, top)


route : BaseParser Route
route =
    Url.oneOf
        (baseRoutes ++ allResourceUrls ++ businessEmployeesResourceRoute ++ assignmentResourceRoute)


allResourceUrls : List (BaseParser a)
allResourceUrls =
    mapUrls [ complianceRoute ]


baseRoutes : List (BaseParser Route)
baseRoutes =
    [ Url.map HomeRoute top
    , Url.map AdminHomeRoute top
    , Url.map ArchiveRoute (Url.s "archives" </> Url.string)
    , Url.map ActivityRoute (Url.s "activity")
    , Url.map AdminDocumentTemplatesRoute (Url.s "a" </> Url.s "documentTemplates")
    , Url.map UsersRoute (Url.s "a" </> Url.s "users")
    , Url.map BadgesRoute (Url.s "a" </> Url.s "badges")
    , Url.map BusinessesRoute (Url.s "a" </> Url.s "businesses")
    , Url.map VersionsRoute (Url.s "a" </> Url.s "versions")
    , Url.map LicenseBadgesRoute (Url.s "employees")
    , Url.map ComplianceRoute (Url.s "compliance")
    , Url.map DocumentsRoute (Url.s "documents" </> Url.int)
    , Url.map ChecklistsRoute (Url.s "checklists")
    , Url.map ChecklistsEditRoute (Url.s "checklists" </> Url.int </> Url.s "edit")
    , Url.map ChecklistsNewRoute (Url.s "checklists" </> Url.s "new")
    , Url.map ChecklistRoute (Url.s "checklists" </> Url.int)
    , Url.map ChecklistEntryRoute (Url.s "checklists" </> Url.int </> Url.s "entries" </> Url.int)
    , Url.map ProfileRoute (Url.s "profile")
    , Url.map StyleGuideRoute (Url.s "style")
    , Url.map NotificationsRoute (Url.s "notifications")
    , Url.map LicenseRenewalRoute (Url.s "licenseRenewal")
    , Url.map AccountRoute (Url.s "account")
    , Url.map AnnouncementRoute (Url.s "announcement")
    , Url.map (BusinessRoute EditBusinessProfile) (Url.s "business" </> Url.s "profile")
    ]


type Route
    = HomeRoute
    | AdminHomeRoute
    | AdminDocumentTemplatesRoute
    | ActivityRoute
    | ArchiveRoute String
    | UsersRoute
    | BadgesRoute
    | DocumentsRoute DocumentTypeId
    | ChecklistsRoute
    | LicenseRenewalRoute
    | ChecklistsEditRoute Int
    | ChecklistsNewRoute
    | ChecklistRoute Int
    | ChecklistEntryRoute Int Int
    | BusinessesRoute
    | LicenseBadgesRoute
    | LogRoute LogPage CrudRoute
    | VersionsRoute
    | ProfileRoute
    | NotificationsRoute
    | StyleGuideRoute
    | AccountRoute
    | AnnouncementRoute
    | ComplianceRoute
    | BusinessRoute BusinessPageTab
    | AssignmentManager CrudRoute


type BusinessPageTab
    = EditBusinessProfile
    | EmployeeProfiles CrudRoute


type LogPage
    = GreenWasteLog
    | PlantWasteLog
    | PackageAdjustmentLog
    | AlarmEventLog
    | Manifest
    | SecurityAccessLog
    | SystemActivityMaintenanceLog
    | MetrcErrorNotificationLog
    | AdEntry
    | VisitorSignIn
    | ExtendedPlantCount
    | Vendor


type CrudRoute
    = Index
    | Show Int
    | Edit Int
    | New


type Resource
    = ResourceC
        { show : Maybe (Int -> Route)
        , edit : Maybe (Int -> Route)
        , new : Maybe Route
        , baseUrl : List String
        , index : Route
        , children : List Resource
        }


type alias BaseParser a =
    Url.Parser (Route -> a) a


makeDefaultResourceRoutes : List String -> (CrudRoute -> Route) -> List (BaseParser Route)
makeDefaultResourceRoutes urlList crudRoute =
    let
        baseUrl =
            arrayToBaseUrl urlList
    in
    [ Url.map (\u -> crudRoute (Show u)) (baseUrl </> Url.int)
    , Url.map (\u -> crudRoute (Edit u)) (baseUrl </> Url.int </> Url.s "edit")
    , Url.map (crudRoute Index) baseUrl
    , Url.map (crudRoute New) (baseUrl </> Url.s "new")
    ]


arrayToBaseUrl : List String -> Url.Parser a a
arrayToBaseUrl urls =
    List.foldr ((</>) << Url.s) Url.top urls


businessEmployeesResourceRoute : List (BaseParser Route)
businessEmployeesResourceRoute =
    makeDefaultResourceRoutes [ "business", "employees" ] (\crudRoute -> BusinessRoute (EmployeeProfiles crudRoute))


assignmentResourceRoute : List (BaseParser Route)
assignmentResourceRoute =
    makeDefaultResourceRoutes [ "assignmentManager" ] AssignmentManager


complianceRoute : Resource
complianceRoute =
    ResourceC
        { show = Nothing
        , edit = Nothing
        , new = Nothing
        , baseUrl = [ "compliance" ]
        , index = ComplianceRoute
        , children =
            complianceChildRoutes
        }


complianceChildRoutes : List Resource
complianceChildRoutes =
    [ manifestResource
    , vendorResource
    , alarmEventLogResource
    , greenWasteLogResource
    , plantWasteLogResource
    , packageAdjustmentLogResource
    , securityAccessLogResource
    , systemActivityMaintenanceLogResource
    , metrcErrorNotificationLogResource
    , adEntryResource
    , visitorSignInResource
    , extendedPlantCountResource
    ]


manifestResource : Resource
manifestResource =
    makeLogResource "manifests" Manifest


vendorResource : Resource
vendorResource =
    makeLogResource "vendors" Vendor


alarmEventLogResource : Resource
alarmEventLogResource =
    makeLogResource "alarmEventLog" AlarmEventLog


adEntryResource : Resource
adEntryResource =
    makeLogResource "advertisingLog" AdEntry


visitorSignInResource : Resource
visitorSignInResource =
    makeLogResource "visitorLog" VisitorSignIn


businessEmployeeResource : Resource
businessEmployeeResource =
    makeDefaultCRUDResource [ "business", "employees" ] (\b -> BusinessRoute (EmployeeProfiles b))


assignmentManagerResource : Resource
assignmentManagerResource =
    makeDefaultCRUDResource [ "assignmentManager" ] AssignmentManager


makeDefaultCRUDResource : List String -> (CrudRoute -> Route) -> Resource
makeDefaultCRUDResource baseUrl crud =
    ResourceC
        { show = Just (\id -> crud (Show id))
        , edit = Just (\id -> crud (Edit id))
        , new = Just (crud New)
        , index = crud Index
        , baseUrl = baseUrl
        , children = []
        }


makeLogResource : String -> LogPage -> Resource
makeLogResource baseUrl logPage =
    makeDefaultCRUDResource [ baseUrl ] (LogRoute logPage)


greenWasteLogResource : Resource
greenWasteLogResource =
    makeLogResource "greenWasteLog" GreenWasteLog


activityResource : Resource
activityResource =
    ResourceC
        { show = Nothing
        , edit = Nothing
        , new = Nothing
        , index = ActivityRoute
        , baseUrl = [ "activity" ]
        , children = []
        }


plantWasteLogResource : Resource
plantWasteLogResource =
    makeLogResource "plantWasteLog" PlantWasteLog


packageAdjustmentLogResource : Resource
packageAdjustmentLogResource =
    makeLogResource "packageAdjustmentLog" PackageAdjustmentLog


securityAccessLogResource : Resource
securityAccessLogResource =
    makeLogResource "securityAccessLog" SecurityAccessLog


systemActivityMaintenanceLogResource : Resource
systemActivityMaintenanceLogResource =
    makeLogResource "systemActivityMaintenanceLog" SystemActivityMaintenanceLog


metrcErrorNotificationLogResource : Resource
metrcErrorNotificationLogResource =
    makeLogResource "metrcErrorNotificationLog" MetrcErrorNotificationLog


extendedPlantCountResource : Resource
extendedPlantCountResource =
    makeLogResource "extendedPlantCount" ExtendedPlantCount


mapUrls : List Resource -> List (BaseParser a)
mapUrls resources =
    resources
        |> List.map mapResourceToUrl
        |> List.concatMap identity


mapResourceToUrl : Resource -> List (BaseParser a)
mapResourceToUrl (ResourceC resource) =
    let
        parents =
            List.concatMap identity
                [ getEditUrl (ResourceC resource)
                , getShowUrl (ResourceC resource)
                , getNewUrl (ResourceC resource)
                , getIndexUrl (ResourceC resource)
                ]

        children =
            mapUrls resource.children
    in
    parents ++ children


singleUrlB : Resource -> Url.Parser (Int -> a) a
singleUrlB resource =
    getBaseUrl resource </> Url.int


getBaseUrl : Resource -> Url.Parser a a
getBaseUrl (ResourceC resource) =
    arrayToBaseUrl resource.baseUrl


getIndexUrl : Resource -> List (BaseParser a)
getIndexUrl (ResourceC resource) =
    [ Url.map resource.index (getBaseUrl (ResourceC resource)) ]


getShowUrl : Resource -> List (BaseParser a)
getShowUrl (ResourceC resource) =
    resource.show
        |> Maybe.map (makeShow (ResourceC resource))
        |> Maybe.withDefault []


getEditUrl : Resource -> List (BaseParser a)
getEditUrl (ResourceC resource) =
    resource.edit
        |> Maybe.map (makeEdit (ResourceC resource))
        |> Maybe.withDefault []


makeEdit : Resource -> (Int -> a) -> List (Url.Parser (a -> c) c)
makeEdit resource edit =
    [ Url.map
        edit
        (singleUrlB resource </> Url.s "edit")
    ]


makeShow : Resource -> (Int -> a) -> List (Url.Parser (a -> c) c)
makeShow resource withId =
    [ Url.map
        withId
        (singleUrlB resource)
    ]


getNewUrl : Resource -> List (BaseParser a)
getNewUrl (ResourceC resource) =
    resource.new
        |> Maybe.map (makeNew (ResourceC resource))
        |> Maybe.withDefault []


makeNew : Resource -> a -> List (Url.Parser (a -> c) c)
makeNew resource new =
    [ Url.map
        new
        (getBaseUrl resource </> Url.s "new")
    ]



-- STRING BASED URLS


getResourceStringUrl : Resource -> CrudRoute -> String
getResourceStringUrl (ResourceC resource) crudRoute =
    let
        baseUrl =
            "/" ++ arrayToUrlString resource.baseUrl

        extension =
            case crudRoute of
                Edit id ->
                    "/" ++ toString id ++ "/edit"

                Show id ->
                    "/" ++ toString id

                New ->
                    "/new"

                Index ->
                    ""
    in
    baseUrl ++ extension


arrayToUrlString : List String -> String
arrayToUrlString urls =
    String.join "/" urls
