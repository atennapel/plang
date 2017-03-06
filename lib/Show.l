type Show [t] {
  Show [(Arr t Str)]
};

let show (\\d -> case {Show id} d);

ilet floatShow (Show floatToString);

ilet strShow (Show id);

{
  Show Show
  show show

  implicit_floatShow floatShow
  implicit_strShow strShow
}
