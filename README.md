Cryptography in Chisel
=======================

Acest proiect are ca scop implementarea în Chisel a unor funcții criptografice fundamentale (AES-128, AES-CTR, AES-CMAC și SHA-256). Modulele sunt gândite ca implementări hardware, respectând specificațiile standardelor oficiale, și pot fi utilizate atât pentru înțelegerea arhitecturii algoritmilor criptografici, cât și ca blocuri de bază în sisteme de securitate hardware.

## Functii

### AES
Am implementat varianta pe 128 de biți a algoritmului AES (Advanced Encryption Standard), cu suport pentru criptare și decriptare, conform specificației
https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.197.pdf.
* Dimensiune bloc: 128 biți (16 bytes)
* Operații implementate: SubBytes, ShiftRows, MixColumns, AddRoundKey, precum și inversele acestora pentru decriptare
* Număr de runde: 10 (conform standardului AES-128)

### AES-CMAC
Acest modul implementează CMAC (Cipher-based Message Authentication Code) conform https://www.rfc-editor.org/rfc/rfc4493.html#section-2.3
, utilizând nucleul AES-128 realizat anterior.

* Dimensiune bloc: 128 biți (16 bytes)
* Chei derivate: cele 2 chei K1 si K2 au fost generate cu ajutorul algoritmului din sursa, pe baza cheii AES inițiale.
* Lungime variabilă a mesajului: suportă mesaje de orice dimensiune, ultimului bloc se aplica padding daca este imcomplet
*Rezultat: un cod de autentificare (MAC) de 128 biți.

### AES-CTR (Counter Mode)
Acest modul implementează AES în modul CTR (Counter Mode), folosind nucleul AES-128 realizat anterior.
* Dimensiune bloc: 128 biți (16 bytes)
* IV: 128 biți împărțiți în nonce (12 bytes) și counter (4 bytes, big-endian).

#### Funcționare

* La fiecare bloc, AES este rulat pe IV curent pentru a genera keystream-ul.
* Blocul de date (blockIn) este XOR-at cu keystream-ul pentru a produce outBlock.
* Counter-ul este incrementat automat în IV.
* Ultimul bloc poate fi mai scurt de 16 bytes – doar primii lastBytes octeți sunt utilizați.

### SHA256

Am implementat algoritmul SHA-256 conform specificației [FIPS 180-4](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)
.
* Dimensiune bloc: 512 biți (64 bytes)
* Dimensiune digest: 256 biți (8 × 32-bit words)
* Padding standard SHA-256 (0x80 … length).

## Site-uri folosite pentru a verifica corectitudinea

* AES - https://testprotect.com/appendix/AEScalc
* AES-CMAC - https://www.lddgo.net/en/encrypt/cmac-calculate
* AES-CTR - https://cryptii.com/pipes/aes-encryption
* SHA256 - https://emn178.github.io/online-tools/sha256.html