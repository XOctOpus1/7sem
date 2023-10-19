import Graphics.GD

-- Функція для приховування інформації у GIF-файлі
hideDataInGIF :: FilePath -> FilePath -> String -> IO ()
hideDataInGIF inputPath outputPath dataToHide = do
    -- Відкриваємо GIF-файл для читання
    image <- loadGifFile inputPath

    -- Проходимося по кожному пікселю і приховуємо інформацію у останньому значущому біті
    let imageData = imageData image
    let hiddenImageData = hideData imageData dataToHide

    -- Створюємо новий GIF-зображення зі спрятаною інформацією
    let hiddenImage = ImageGif (imageSize image) hiddenImageData

    -- Зберігаємо зображення зі спрятаною інформацією
    saveGifFile outputPath hiddenImage

-- Функція для приховування інформації у пікселях
hideData :: [Pixel] -> String -> [Pixel]
hideData pixels [] = pixels
hideData [] _ = []
hideData (p:ps) (d:ds) = hideInPixel p d : hideData ps ds

-- Функція для приховування біта у пікселі
hideInPixel :: Pixel -> Char -> Pixel
hideInPixel (Pixel r g b) bit = Pixel (setBit r 0 (read [bit])) g b

main :: IO ()
main = do
    let inputPath = "input.gif"  -- Шлях до вхідного GIF-файлу
    let outputPath = "output.gif"  -- Шлях до вихідного GIF-файлу
    let dataToHide = "This is a secret message!"  -- Інформація, яку потрібно приховати

    hideDataInGIF inputPath outputPath dataToHide
