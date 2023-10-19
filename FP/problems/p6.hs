import Text.XML.Light

-- Функція для створення XML документа
createXMLDocument :: Element
createXMLDocument =
  Element
    (unqual "root")      -- Назва кореневого елемента
    []
    [ Element (unqual "element1") [] [Text (CData CDataText "Some text" Nothing)]
    , Element (unqual "element2") [] []
    ]

-- Функція для створення DTD визначення
createDTD :: String
createDTD = "<!ELEMENT root (element1, element2)>\n<!ELEMENT element1 (#PCDATA)>\n<!ELEMENT element2 EMPTY>"

main :: IO ()
main = do
  let xmlDocument = Document (Prologue [] Nothing []) createXMLDocument []
  writeFile "example.xml" $ showTopElement xmlDocument
  writeFile "example.dtd" createDTD