{ mkDerivation, base, bytestring, deepseq, hashable, HUnit, stdenv
, test-framework, test-framework-hunit, text, semigroups

, criterion
}:
mkDerivation {
  pname = "case-insensitive";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base bytestring deepseq hashable text ];
  testHaskellDepends = [
    base bytestring HUnit test-framework test-framework-hunit text semigroups

    criterion deepseq
  ];
  homepage = "https://github.com/basvandijk/case-insensitive";
  description = "Case insensitive string comparison";
  license = stdenv.lib.licenses.bsd3;
}
