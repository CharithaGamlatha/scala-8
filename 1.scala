object CaesarCipher {

  def encrypt(plaintext: String, shift: Int): String = {
    plaintext.map {
      case c if c.isLetter =>
        val shiftBase = if (c.isUpper) 'A' else 'a'
        ((c - shiftBase + shift) % 26 + shiftBase).toChar
      case c => c
    }
  }

  def decrypt(ciphertext: String, shift: Int): String = {
    encrypt(ciphertext, -shift)
  }

  def cipher(text: String, shift: Int, operation: String): String = {
    operation match {
      case "encrypt" => encrypt(text, shift)
      case "decrypt" => decrypt(text, shift)
      case _ => throw new IllegalArgumentException("Operation must be 'encrypt' or 'decrypt'")
    }
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"
    val shift = 3

    val encryptedText = cipher(plaintext, shift, "encrypt")
    println(s"Encrypted: $encryptedText")

    val decryptedText = cipher(encryptedText, shift, "decrypt")
    println(s"Decrypted: $decryptedText")
  }
}
