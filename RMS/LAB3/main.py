import cv2
import numpy as np
import pywt


def dct_compress(image, block_size=8, quality=10):
    # Розбиваємо зображення на блоки
    rows, cols = image.shape
    blocks = [image[i:i+block_size, j:j+block_size] for i in range(0, rows, block_size) for j in range(0, cols, block_size)]

    # Застосовуємо DCT до кожного блоку
    dct_blocks = [cv2.dct(np.float32(block)) for block in blocks]

    # Кількість коефіцієнтів для стиснення
    num_coefficients = int((100 - quality) / 100 * (block_size ** 2))

    # Занулюємо менш важливі коефіцієнти
    for block in dct_blocks:
        block.flat[num_coefficients:] = 0

    # Застосовуємо обернене DCT до кожного блоку
    idct_blocks = [cv2.idct(block) for block in dct_blocks]

    # Збираємо зображення зі стисненими блоками
    compressed_image = np.zeros_like(image)
    for i, block in enumerate(idct_blocks):
        r, c = divmod(i, cols // block_size)
        compressed_image[r*block_size:(r+1)*block_size, c*block_size:(c+1)*block_size] = block

    return compressed_image


def haarcwt_compress(image, level=1):
    # Apply Haar wavelet transform
    LL, (LH, HL, HH) = pywt.dwt2(image, 'haar')

    # Discard details for compression
    for _ in range(level):
        LH[:,:] = 0
        HL[:,:] = 0
        HH[:,:] = 0

    # Apply inverse Haar wavelet transform
    compressed_image = pywt.idwt2((LL, (LH, HL, HH)), 'haar')

    # Convert to uint8 for display
    compressed_image = np.uint8(compressed_image)

    return compressed_image


def calculate_psnr(original, compressed):
    mse = np.mean((original - compressed) ** 2)
    psnr = 20 * np.log10(255 / np.sqrt(mse))
    return psnr


# Завантаження зображення
image_path = 'ico.png'  # Замініть шлях до свого зображення
original_image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)

# Check if the image was successfully loaded
if original_image is None:
    print(f"Error: Unable to load the image from {image_path}")
else:
    # Continue with the compression and PSNR calculation
    compressed_dct = dct_compress(original_image, block_size=8, quality=10)
    compressed_wavelet = haarcwt_compress(original_image, level=1)

    cv2.imshow('Original Image', original_image)
    cv2.imshow('DCT Compressed Image', compressed_dct)
    cv2.imshow('Wavelet Compressed Image', compressed_wavelet)

    psnr_dct = calculate_psnr(original_image, compressed_dct)
    psnr_wavelet = calculate_psnr(original_image, compressed_wavelet)

    print(f'PSNR (DCT): {psnr_dct:.2f} dB')
    print(f'PSNR (Wavelet): {psnr_wavelet:.2f} dB')

    cv2.waitKey(0)
    cv2.destroyAllWindows()
