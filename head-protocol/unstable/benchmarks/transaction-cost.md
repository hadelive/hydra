--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-23 03:57:19.536571224 UTC |
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
| 1| 5836 | 10.76 | 3.42 | 0.52 |
| 2| 6035 | 12.53 | 3.97 | 0.55 |
| 3| 6236 | 14.59 | 4.61 | 0.58 |
| 5| 6638 | 18.60 | 5.87 | 0.63 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14283 | 98.85 | 30.89 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.33 | 9.66 | 0.52 |
| 3 | 170 | 751 | 43.91 | 12.57 | 0.63 |
| 4 | 227 | 858 | 49.68 | 14.37 | 0.69 |
| 5 | 282 | 969 | 62.87 | 17.92 | 0.83 |
| 6 | 340 | 1081 | 74.85 | 21.15 | 0.96 |
| 7 | 393 | 1192 | 86.55 | 24.35 | 1.08 |
| 8 | 450 | 1303 | 90.12 | 25.75 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1783 | 24.29 | 7.69 | 0.48 |
| 2| 1878 | 24.85 | 8.50 | 0.49 |
| 3| 2065 | 27.31 | 9.86 | 0.53 |
| 5| 2439 | 31.88 | 12.49 | 0.61 |
| 10| 3174 | 41.07 | 18.37 | 0.75 |
| 40| 7615 | 99.65 | 54.67 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 621 | 22.57 | 7.31 | 0.41 |
| 2| 739 | 23.65 | 8.24 | 0.43 |
| 3| 1022 | 27.98 | 10.15 | 0.49 |
| 5| 1320 | 32.07 | 12.62 | 0.56 |
| 10| 2128 | 42.79 | 18.95 | 0.73 |
| 39| 6268 | 91.19 | 51.74 | 1.54 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 29.13 | 8.90 | 0.48 |
| 2| 879 | 29.89 | 9.82 | 0.50 |
| 3| 1006 | 31.57 | 10.95 | 0.53 |
| 5| 1237 | 34.30 | 13.02 | 0.58 |
| 10| 1823 | 45.38 | 19.41 | 0.74 |
| 35| 5530 | 97.95 | 50.76 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 684 | 33.83 | 10.15 | 0.53 |
| 2| 761 | 35.21 | 11.18 | 0.55 |
| 3| 996 | 38.62 | 12.83 | 0.60 |
| 5| 1219 | 41.78 | 15.02 | 0.65 |
| 10| 2004 | 54.17 | 21.84 | 0.83 |
| 29| 4910 | 99.54 | 47.19 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5800 | 27.00 | 9.06 | 0.69 |
| 2| 5937 | 36.11 | 12.14 | 0.79 |
| 3| 6102 | 45.99 | 15.48 | 0.90 |
| 4| 6143 | 50.41 | 16.91 | 0.95 |
| 5| 6329 | 60.01 | 20.21 | 1.06 |
| 6| 6483 | 66.54 | 22.38 | 1.13 |
| 7| 6841 | 85.25 | 28.83 | 1.35 |
| 8| 6791 | 86.18 | 28.97 | 1.35 |
| 9| 6918 | 98.65 | 33.28 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.52 | 6.86 | 0.62 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 569 | 6173 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1138 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 38 | 2165 | 7127 | 96.44 | 36.92 | 1.51 |

