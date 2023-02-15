# Revision history for hspec-golden-cereal

## 0.4.2.0  -- 2023-01-24
* Revert: Compatibility mode as default. 
* New default: Byte For Byte

## 0.4.1.0  -- 2023-01-04
* Compatibility mode as default (instead of Byte For Byte)

## 0.4.0.1  -- 2023-01-04
* Logs about mode in use

## 0.4.0.0  -- 2023-01-03
* Introduce COMPATIBILITY_CHECK mode

## 0.3.0.0  -- 2022-12-16
* Remove roundtripFromFile, but re use their code in order to fix goldenSpecs

## 0.2.0.0  -- 2022-12-15
* Add roundtripFromFile. Run golden tests without using the Arbitrary instance. Will decode the golden file and test if the encoding function is equivalent to the golden file 

## 0.1.0.0  -- 2021-04-11
* First version adapted from hspec-golden-aeson. 
