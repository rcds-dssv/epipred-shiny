NGLVieweR(pdbfile)  %>%
  addRepresentation(
    "cartoon",
    param = list(name = "cartoon", color = "residueindex")
  ) %>%
  addRepresentation("ball+stick",
    param = list(
      colorScheme = "element",
      colorValue = "yellow",
      sele = "10"
    )
  ) %>%
  addRepresentation("surface",
    param = list(
      isolevel = 0.005, isolevelType = "value",
      sele = "10",
      color = "0x4E2A84", colorMode = "rgb"
    )
  ) %>%
  addRepresentation("label"
    
  )



# "atomname", "atomindex", "occupancy", "bfactor", 
# "serial", "element", "atom", "resname", "resno",
# "res", "text", "qualified". When set to "text", the
# `labelText` list is used.


