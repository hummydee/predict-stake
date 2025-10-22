# PredictStake - Prediction Market with Reward Pool

A Clarity smart contract for Stacks that enables decentralized prediction markets with multi-outcome support, reward pools, and pro-rata payouts.

## Overview

PredictStake allows users to create and participate in prediction markets where participants place STX bets on outcomes before a deadline. 
Market creators finalize markets after the deadline, and winners claim pro-rata payouts from the combined pool of total bets, protocol fees, and deposited rewards.

## Features

- **Multi-Outcome Markets**: Support for binary and multi-outcome prediction markets
- **Configurable Deadlines**: Set custom block-height deadlines for each market
- **STX Betting**: Users place STX bets on specific outcomes before market deadline
- **Reward Pool**: Anyone can deposit additional STX to increase winner payouts
- **Protocol Fees**: Configurable basis points (bps) fee collected on market finalization
- **Pro-Rata Payouts**: Winners receive proportional share of (total_bets - fee + reward_pool)
- **Market Cancellation**: Creators can cancel markets before resolution for full refunds
- **Admin Controls**: Role-based access control with admin privilege transfer
- **Comprehensive Error Handling**: 7 distinct error codes for debugging and monitoring

## Installation

### Prerequisites
- Stacks blockchain environment
- Clarity contract deployment tools (Clarinet recommended)
