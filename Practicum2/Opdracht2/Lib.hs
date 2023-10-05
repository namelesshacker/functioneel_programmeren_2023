


module Lib
    ( someFunc,euclid
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

euclid::Integer->Integer->Integer
euclid n p
  | n < 0            = 0
  | n `mod` 17 == 2  = -15
  | otherwise        = n + p

--rest = print $ (euclid 2 3)





--main :: IO ()
--main = putStrLn "Test suite not yet implemented"


-- https://serokell.io/blog/introduction-to-template-haskell

-- http://wiki.haskell.org/Template_Haskell

-- https://downloads.haskell.org/~ghc/7.0.2/docs/html/users_guide/template-haskell.html


-- https://srid.ca/haskell-template

-- https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html


-- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/

-- https://github.com/PHPirates/haskell-template-project/blob/master/Setup.hs


-- https://www.joachim-breitner.de/blog/772-Template_Haskell_recompilation

 
-- https://downloads.haskell.org/~ghc/6.0/docs/html/users_guide/template-haskell.html



-- Problem 31: Determine whether a given integer number is prime.
-- Armed with our experience from Project Euler, this is not very daunting.
-- We make sure n is not divided by any "candidates" up to sqrt(n). Which candidates?
-- Well, the prime numbers, of course! Well, we're not gonna spend time tracking out the
-- prime numbers in a primality test, though - we're going to use the P6 candidates;
-- numbers of the form 6*n±1 :
isprime :: (Integral a) => a -> Bool
isprime n | n < 4 = n > 1
isprime n = all ((/=0) . mod n) $ takeWhile (<=m) candidates
    where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1, 1]])
          m = floor . sqrt $ fromIntegral n

-- Problem 32: Determine the greatest common divisor of two positive integers.
-- Enter Euclid:
mygcd a 0 = abs a
mygcd a b = mygcd b (a `mod` b)

-- Problem 33: Determine whether two positive integer numbers are coprime.
-- Straight from the definition of coprimality:
coprime a b = gcd a b == 1

-- Problem 34: Calculate Euler's totient function.
-- The definition makes it easy for us, again:
phi n = length [m | m <- [1..n-1], coprime m n]


------
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

main :: IO ()
main = mapM_ print [13 `modInv`7344, 42 `modInv` 2017]


{-
https://github.com/jaybosamiya/DiffieHellman-ManInTheMiddle/blob/master/diffie_hellman.py
https://wiremask.eu/articles/diffie-hellman-man-in-the-middle-attack/
https://eprints.illc.uva.nl/id/eprint/934/1/MoL-2014-16.text.pdf



Man-in-the-middle attack Java code
https://asecuritysite.com/keyexchange/diffie_crack
https://guides.codepath.com/websecurity/Man-in-the-Middle
https://att-innovate.github.io/squanch/demos/man-in-the-middle.html
https://www.varonis.com/blog/man-in-the-middle-attack
https://www.chegg.com/homework-help/questions-and-answers/simulate-man-middle-attack-diffie-hellman-python-completing-existing-code-ross-trying-inte-q90983650
https://medium.com/asecuritysite-when-bob-met-alice/well-done-to-whatsapp-in-taking-security-seriously-key-transparency-2d56bce47396
https://codeberg.org/severin/vula/src/branch/main/podman/Vula-MitM-tool.md
https://www.rapid7.com/fundamentals/man-in-the-middle-attacks/
https://www.esat.kuleuven.be/cosic/publications/article-2698.pdf
https://www.dschoni.de/security/54/
https://www.linkedin.com/pulse/meet-first-couple-cryptography-alice-bob-mike-makwani
https://crysp.uwaterloo.ca/courses/cs458/F08-lectures/Module5.pdf
https://intronetworks.cs.luc.edu/current/html/publickey.html
https://www.comparitech.com/vpn/what-is-a-man-in-the-middle-attack/
https://textbook.cs161.org/crypto/certificates.html
http://www2.ic.uff.br/~michael/kr1999/7-security/7_03-authentication.htm
https://www.net.in.tum.de/pub/netsec2016/10_cryptoprot.pdf
https://www.synopsys.com/blogs/software-security/man-in-the-middle-mitm-attack.html
https://www.cs.du.edu/~ramki/courses/security/forensics/notes/NetworkSecurity.pdf
https://github.com/kientuong114/docker-mitm
http://www.cs.toronto.edu/~ahchinaei/teaching/2016jan/csc358/Lecture12-6p.pdf
https://people.cs.rutgers.edu/~pxk/419/notes/pdf/08-auth-slides.pdf


Diffie-Hellman (Man-in-the-middle) haskell
https://inria.hal.science/hal-01948964/document
https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=man
https://chaum.com/wp-content/uploads/2022/01/MitMCryptologia.pdf
https://www.academia.edu/70628631/The_State_of_the_art_Cryptographic_Algorithms
https://dl.acm.org/profile/81100070918/publications?Role=author
https://pure.tue.nl/ws/files/2425555/200612074.pdf
https://www.diva-portal.org/smash/get/diva2:1566787/FULLTEXT01.pdf
https://www.ndss-symposium.org/wp-content/uploads/2017/09/ndss2017_04A-3_Donenfeld_paper.pdf
https://www.mdpi.com/2410-387X/2/4
https://www.il-pib.pl/czasopisma/JTIT/2021/2/107.pdf
https://discovery.dundee.ac.uk/ws/portalfiles/portal/39889676/paper.pdf
http://www.illusorytls.com/docs/illusoryTLS_zeronights2015_ndd.pdf
https://researchmgt.monash.edu/ws/portalfiles/portal/348644028/342377358_oa.pdf
https://members.loria.fr/CDumenil/files/RapportDeStage.pdf
https://www.research-collection.ethz.ch/bitstream/handle/20.500.11850/569146/1/Wiesner_Sven.pdf
https://diglib.tugraz.at/download.php?id=60a4eadb05681&location=browse
https://www.usenix.org/system/files/conference/usenixsecurity15/sec15-paper-kaloper-mersinjak.pdf
https://isyou.info/jowua/papers/jowua-v8n1-3.pdf
https://computerscience.paris/security/pdf/readings/WireGuard%20Next%20Generation%20Network%20Tunnel.pdf
https://archive.conference.hitb.org/hitbsecconf2015ams/wp-content/uploads/2014/12/WHITEPAPER-The-illusoryTLS-Asymmetric-Backdoor.pdf
https://sar.informatik.hu-berlin.de/research/publications/SAR-PR-2020-01/SAR-PR-2020-01_.pdf
https://blog.srinivasan.biz/software/security/crypto-strength-pre-and-post-quantum
https://crypto.stackexchange.com/questions/72370/diffie-hellman-algorithm-and-mitm-attack
https://github.com/topics/diffie-hellman-algorithm?o=desc&s=updated
https://hackage.haskell.org/package/cpsa-2.2.6/changelog
https://www.researchgate.net/publication/4023175_The_Diffie-Hellman_key-agreement_scheme_in_the_strand-space_model
https://unhandledexpression.com/page5/

-}