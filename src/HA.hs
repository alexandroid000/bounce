{-# LANGUAGE NoMonomorphismRestriction  #-}

module HA
    (
      Location (..)
    , Transition (..)
    , Assignment (..)
    , Param (..)
    , write_HA
    , HA (..)
    ) where
        
import  Data.List

type Id = Int
type Source = Id
type Target = Id
type Label = String
type Invariant = String
type Flow = String
type Guard = String
type Assignment = String

data Param = RealConst Label | RealDyn Label | Lab Label

data Location = Location Id Label Invariant Flow

data Transition = Transition Source Target Label Guard Assignment
    deriving (Show)

data HA = HA    { name :: Label
                , params :: [Param]
                , locations :: [Location]
                , transitions :: [Transition]
                }

write_param :: Param -> String
write_param (RealDyn name) = "<param name=\""++name++"\" type=\"real\" local=\"true\" d1=\"1\" d2=\"1\" dynamics=\"any\" />"
write_param (RealConst name) = "<param name=\""++name++"\" type=\"real\" local=\"true\" d1=\"1\" d2=\"1\" dynamics=\"const\" />"
write_param (Lab name) = "<param name=\""++name++"\" type=\"label\" local=\"false\" />"

write_params :: [Param] -> String
write_params [] = ""
write_params (p:ps) = "\t" ++ (write_param p) ++ "\n" ++ (write_params ps)

write_loc :: Location -> String
write_loc (Location id name inv flow) =
    "<location id=\""++(show id)++"\" \
    \name=\""++name++"\">\n \
    \\t\t<invariant>"++inv++"</invariant>\n\
    \\t\t<flow>"++flow++"</flow>\n\
    \\t</location>"

write_locs :: [Location] -> String
write_locs [] = ""
write_locs (l:ls) = "\t" ++ (write_loc l) ++ "\n" ++ (write_locs ls)

--data Transition = Transition Source Target Label Guard Assignment
write_tran :: Transition -> String
write_tran (Transition source target label guard assign) =
    "<transition source=\""++(show source)++"\" target=\""++(show
    target)++"\" asap=\"true\" >\n\
    \\t\t<label>"++label++"</label>\n\
    \\t\t<guard>"++guard++"</guard>\n\
    \\t\t<assignment>"++assign++"</assignment>\n\
    \\t</transition>"

write_trans :: [Transition] -> String
write_trans [] = ""
write_trans (t:ts) = "\t" ++ (write_tran t) ++ "\n" ++ (write_trans ts)

form_HA :: HA -> String
form_HA ha = let
    header = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n\
    \<sspaceex xmlns=\"http://www-verimag.imag.fr/xml-namespaces/sspaceex\"\
    \version=\"0.2\" math=\"SpaceEx\">\n\
    \\t<component id=\""++(name ha)++"\">\n"
    footer = "</component>\n</sspaceex>"
    xml_params = write_params (params ha)
    xml_loc = write_locs (locations ha)
    xml_trns = write_trans (transitions ha)
    in header++xml_params++xml_loc++xml_trns++footer

write_HA :: HA -> IO ()
write_HA ha = let
    fname = (name ha)++".xml"
    xml = form_HA ha
    in writeFile fname xml
