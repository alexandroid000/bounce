{-# LANGUAGE NoMonomorphismRestriction  #-}

module HA
    (
      Location (..)
    , Transition (..)
    , write_HA
    , test_ha
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
type Param = String


data Location = Location Id Label Invariant Flow

data Transition = Transition Source Target Label Guard Assignment

data HA = HA    { name :: Label
                , params :: [Param]
                , locations :: [Location]
                , transitions :: [Transition]
                }

write_param :: Param -> String
write_param name = "<param name=\""++name++"\" type=\"real\" local=\"true\" d1=\"1\" d2=\"1\" dynamics=\"any\" />"

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
    target)++"\">\n\
    \\t\t<label>"++label++"</label>\n\
    \\t\t<guard>"++guard++"</guard>\n\
    \\t\t<assignment>"++assign++"</assignment>\n\
    \\t</transition>"

write_trans :: [Transition] -> String
write_trans [] = ""
write_trans (t:ts) = "\t" ++ (write_tran t) ++ "\n" ++ (write_trans ts)

loc1 = Location 1 "loc1" "inv" "flow"
loc2 = Location 2 "loc2" "inv" "flow"
t1 = Transition 1 2 "t1" "guard" "assignment"

test_ha :: HA
test_ha = HA    { name="test"
                , params=["p1","p2"]
                , locations=[loc1, loc2]
                , transitions=[t1]}

write_HA :: HA -> String
write_HA ha = let
    header = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n\
    \<sspaceex xmlns=\"http://www-verimag.imag.fr/xml-namespaces/sspaceex\"\
    \version=\"0.2\" math=\"SpaceEx\">\n\
    \\t<component id=\""++(name ha)++"\">\n"
    footer = "</component>\n</sspaceex>"
    xml_params = write_params (params ha)
    xml_loc = write_locs (locations ha)
    xml_trns = write_trans (transitions ha)
    in header++xml_params++xml_loc++xml_trns++footer
