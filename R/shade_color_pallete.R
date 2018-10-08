
# Amino Acid color from "http://life.nthu.edu.tw/~fmhsu/rasframe/SHAPELY.HTM"

# Amino Acid  Color      Triple        Amino Acid    Color  Triple
# ASP,GLU   bright red [230,10,10]     CYS,MET     yellow [230,230,0]
# LYS,ARG   blue       [20,90,255]     SER,THR     orange [250,150,0]
# PHE,TYR   mid blue   [50,50,170]     ASN,GLN     cyan   [0,220,220]
# GLY       light grey [235,235,235]   LEU,VAL,ILE green  [15,130,15]
# ALA       dark grey  [200,200,200]   TRP         pink   [180,90,180]
# HIS       pale blue  [130,130,210]   PRO         flesh  [220,150,130]

aa_color  <-
  c(
    "D" = "#E60A0A", "E" = "#E60A0A",
    "L" = "#145AFF", "R" = "#145AFF",
    "F" = "#3232AA", "Y" = "#3232AA",
    "G" = "#EBEBEB",
    "A" = "#C8C8C8",
    "H" = "#8282D2",
    "C" = "#E6E600", "M" = "#E6E600",
    "S" = "#FA9600", "T" = "#FA9600",
    "N" = "#00DCDC", "Q" = "#00DCDC",
    "L" = "#0F820F", "V" = "#0F820F", "I" = "#0F820F",
    "W" = "#B45AB4",
    "P" = "#DC9682",
    "-" = "#00000000", "NA" = "#00000000"
  )
scales::show_col(aa_color)

dna_color <-
  c(
    "A" = "#FF0000",
    "T" = "#00FF00",
    "G" = "#0000FF",
    "C" = "#646464",
    "-" = "#000000"
  )

# color pallete from bioSyntax "https://github.com/bioSyntax/bioSyntax"
color_nucleotide <-
  c(
    "A"	= "#47FF19",
    "T"	= "#4192FF",
    "G"	= "#FFAF00",
    "C"	= "#FF4641",
    "U"	= "#8A89FF",
    "R"	= "#FFFE80",
    "Y"	= "#E180FF",
    "S"	= "#FF9B80",
    "W"	= "#80FFF2",
    "M"	= "#CE8834",
    "K"	= "#90B82C",
    "D"	= "#C7FFB9",
    "B"	= "#F8C1C0",
    "V"	= "#FFE3B9",
    "H"	= "#BFD8F9",
    "N"	= "#FFFFFF",
    "X"	= "#E6E6E6",
    "-" = "#E6E6E6"
  )

# Amino Acid Coloring (CLUSTAL)
color_aa_clustal <-
  c(
    "A" = "#80A0F0",
    "R" = "#F01505",
    "N" = "#00FF00",
    "D" = "#C048C0",
    "C" = "#F08080",
    "Q" = "#00FF00",
    "E" = "#C048C0",
    "G" = "#F09048",
    "H" = "#15A4A4",
    "I" = "#80A0F0",
    "L" = "#80A0F0",
    "K" = "#F01505",
    "M" = "#80A0F0",
    "F" = "#80A0F0",
    "P" = "#FFFF00",
    "S" = "#00FF00",
    "T" = "#00FF00",
    "W" = "#80A0F0",
    "Y" = "#15A4A4",
    "V" = "#80A0F0",
    "B" = "#FFFFFF",
    "X" = "#FFFFFF",
    "Z" = "#FFFFFF"
  )

# Amino Acid Coloring (Zappo)
color_aa_zappo <-
  c(
    "A"	= "#FFAFAF",
    "R"	= "#6464FF",
    "N"	= "#00FF00",
    "D"	= "#FF0000",
    "C"	= "#FFFF00",
    "Q"	= "#00FF00",
    "E"	= "#FF0000",
    "G"	= "#FF00FF",
    "H"	= "#6464FF",
    "I"	= "#FFAFAF",
    "L"	= "#FFAFAF",
    "K"	= "#6464FF",
    "M"	= "#FFAFAF",
    "F"	= "#FFC800",
    "P"	= "#FF00FF",
    "S"	= "#00FF00",
    "T"	= "#00FF00",
    "W"	= "#FFC800",
    "Y"	= "#FFC800",
    "V"	= "#FFAFAF",
    "B"	= "#FFFFFF",
    "X"	= "#FFFFFF",
    "Z"	= "#FFFFFF"
  )

# Amino Acid Coloring (Taylor)
color_aa_taylor <-
  c(
    "A" = "#CCFF00",
    "R" = "#0000FF",
    "N" = "#CC00FF",
    "D" = "#FF0000",
    "C" = "#FFFF00",
    "Q" = "#FF00CC",
    "E" = "#FF0066",
    "G" = "#FF9900",
    "H" = "#0066FF",
    "I" = "#66FF00",
    "L" = "#33FF00",
    "K" = "#6600FF",
    "M" = "#00FF00",
    "F" = "#00FF66",
    "P" = "#FFCC00",
    "S" = "#FF3300",
    "T" = "#FF6600",
    "W" = "#00CCFF",
    "Y" = "#00FFCC",
    "V" = "#99FF00",
    "B" = "#FFFFFF",
    "X" = "#FFFFFF",
    "Z" = "#FFFFFF"
  )

# Amino Acid Coloring (Hydrophobicity)
color_aa_hydrophobicity <-
  c(
    "A" = "#AD0052",
    "R" = "#0000FF",
    "N" = "#0C00F3",
    "D" = "#0C00F3",
    "C" = "#C2003D",
    "Q" = "#0C00F3",
    "E" = "#0C00F3",
    "G" = "#6A0095",
    "H" = "#1500EA",
    "I" = "#FF0000",
    "L" = "#EA0015",
    "K" = "#0000FF",
    "M" = "#B0004F",
    "F" = "#CB0034",
    "P" = "#4600B9",
    "S" = "#5E00A1",
    "T" = "#61009E",
    "W" = "#5B00A4",
    "Y" = "#4F00B0",
    "V" = "#F60009",
    "B" = "#0C00F3",
    "X" = "#680097",
    "Z" = "#0C00F3"
  )
