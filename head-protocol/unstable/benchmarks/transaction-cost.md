--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-23 03:36:18.427795137 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045 | 2652 | 
| νCommit | 61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c | 685 | 
| νHead | a1442faf26d4ec409e2f62a685c1d4893f8d6bcbaf7bcb59d6fa1340 | 14599 | 
| μHead | fd173b993e12103cd734ca6710d364e17120a5eb37a224c64ab2b188* | 5284 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5834 | 10.61 | 3.37 | 0.52 |
| 2| 6037 | 12.61 | 4.00 | 0.55 |
| 3| 6238 | 14.81 | 4.69 | 0.58 |
| 5| 6640 | 18.84 | 5.95 | 0.64 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 32.19 | 9.36 | 0.51 |
| 3 | 171 | 747 | 43.75 | 12.55 | 0.63 |
| 4 | 226 | 858 | 51.13 | 14.74 | 0.71 |
| 5 | 282 | 969 | 59.24 | 16.99 | 0.79 |
| 6 | 337 | 1081 | 74.55 | 21.04 | 0.95 |
| 7 | 397 | 1192 | 76.94 | 22.14 | 0.98 |
| 8 | 448 | 1303 | 82.91 | 23.92 | 1.05 |
| 9 | 504 | 1414 | 91.71 | 26.48 | 1.14 |
| 10 | 560 | 1525 | 97.97 | 28.46 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.37 | 7.71 | 0.48 |
| 2| 1952 | 25.92 | 8.80 | 0.51 |
| 3| 2079 | 26.98 | 9.78 | 0.53 |
| 5| 2318 | 30.00 | 11.96 | 0.58 |
| 10| 3174 | 42.27 | 18.72 | 0.77 |
| 37| 7508 | 98.78 | 52.45 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 606 | 22.80 | 7.36 | 0.41 |
| 2| 835 | 25.49 | 8.79 | 0.46 |
| 3| 958 | 26.06 | 9.59 | 0.47 |
| 5| 1248 | 30.83 | 12.28 | 0.54 |
| 10| 1955 | 38.84 | 17.82 | 0.68 |
| 39| 6523 | 98.42 | 53.77 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.09 | 8.89 | 0.48 |
| 2| 812 | 29.22 | 9.61 | 0.49 |
| 3| 1036 | 34.18 | 11.67 | 0.56 |
| 5| 1226 | 37.06 | 13.79 | 0.60 |
| 10| 2004 | 47.33 | 20.02 | 0.77 |
| 34| 5818 | 94.91 | 49.48 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.83 | 10.16 | 0.53 |
| 2| 816 | 35.88 | 11.39 | 0.56 |
| 3| 1050 | 39.22 | 13.02 | 0.61 |
| 5| 1358 | 44.15 | 15.73 | 0.68 |
| 10| 2004 | 53.87 | 21.76 | 0.83 |
| 30| 4923 | 98.51 | 47.48 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 26.96 | 9.06 | 0.69 |
| 2| 6007 | 36.98 | 12.45 | 0.80 |
| 3| 5970 | 40.58 | 13.54 | 0.84 |
| 4| 6212 | 51.43 | 17.31 | 0.96 |
| 5| 6575 | 66.02 | 22.29 | 1.13 |
| 6| 6651 | 75.40 | 25.55 | 1.24 |
| 7| 6712 | 81.57 | 27.50 | 1.30 |
| 8| 6984 | 95.84 | 32.40 | 1.47 |
| 9| 6909 | 96.47 | 32.43 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1707 | 6854 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2158 | 7121 | 95.56 | 36.62 | 1.50 |

