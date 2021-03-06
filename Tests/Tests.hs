import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import Data.Serialize
import Data.Vhd.Checksum
import Data.Vhd.Types
import Data.Vhd.Header
import Data.Vhd.Footer
import Data.Vhd.UniqueId
import Data.Vhd.Serialize
import Data.Vhd.Time
import Data.Monoid

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary

instance Arbitrary UniqueId where
    arbitrary = uniqueId . B.pack <$> replicateM 16 arbitrary

instance Arbitrary Cookie where
    arbitrary = cookie . B.pack <$> replicateM 8 arbitrary

instance Arbitrary CreatorApplication where
    arbitrary = creatorApplication . B.pack <$> replicateM 4 arbitrary

instance Arbitrary ParentLocatorEntry where
    arbitrary = ParentLocatorEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ParentLocatorEntries where
    arbitrary = parentLocatorEntries <$> replicateM 8 arbitrary

instance Arbitrary ParentUnicodeName where
    arbitrary = parentUnicodeName <$> replicateM 64 (arbitrary `suchThat` (\w -> w /= '\0'))

instance Arbitrary DiskGeometry where
    arbitrary = DiskGeometry <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockSize where
    arbitrary = return $ BlockSize (2*1024*1024)

instance Arbitrary DiskType where
    arbitrary = elements [ DiskTypeFixed, DiskTypeDynamic, DiskTypeDifferencing ]

instance Arbitrary CreatorHostOs where
    arbitrary = elements [ CreatorHostOsUnknown, CreatorHostOsWindows, CreatorHostOsMacintosh ]

instance Arbitrary VhdDiffTime where
    arbitrary = VhdDiffTime <$> arbitrary

instance Arbitrary Header where
    arbitrary = Header
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (pure $ B.replicate 4 0)
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Checksum where
    arbitrary = return mempty

instance Arbitrary Footer where
    arbitrary = Footer
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

prop_header_marshalling_id :: Header -> Bool
prop_header_marshalling_id h = decode (encode h) == Right h

prop_footer_marshalling_id :: Footer -> Bool
prop_footer_marshalling_id f = decode (encode f) == Right f

marshallingTests =
    [ testProperty "header identity" prop_header_marshalling_id
    , testProperty "footer identity" prop_footer_marshalling_id
    ]

main = defaultMain
    [ testGroup "marshalling" marshallingTests
    ]
