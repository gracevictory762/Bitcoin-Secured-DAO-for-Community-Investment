# 🏦 Bitcoin-Secured DAO for Community Investment
## 🌟 Overview
This smart contract implements a Decentralized Autonomous Organization (DAO) that enables community members to pool funds and collectively decide on investments in local businesses or infrastructure projects. The entire process is managed through Clarity smart contracts on the Stacks blockchain, secured by Bitcoin.

## 🔑 Key Features

- 👥 **Membership System**: Join the DAO by paying a membership fee
- 💰 **Treasury Management**: Pool funds in a secure, contract-controlled treasury
- 📝 **Proposal Creation**: Any member can propose funding for local businesses or projects
- 🗳️ **Voting Mechanism**: Democratic decision-making through on-chain voting
- ⚙️ **Automatic Execution**: Successful proposals are automatically funded

## 📋 Contract Functions

### Membership

- `join-dao()`: Become a member by paying the membership fee
- `contribute-funds(amount)`: Add funds to the DAO treasury

### Proposals

- `create-proposal(title, description, amount, recipient)`: Create a new funding proposal
- `vote-on-proposal(proposal-id, vote)`: Vote yes/no on an active proposal
- `execute-proposal(proposal-id)`: Execute a proposal after voting period ends
- `cancel-proposal(proposal-id)`: Cancel your own proposal if still active

### Administration

- `update-membership-fee(new-fee)`: Update the membership fee (owner only)
- `update-voting-period(new-period)`: Change the voting period duration (owner only)
- `update-execution-threshold(new-threshold)`: Modify the approval threshold (owner only)

### Read-Only Functions

- `get-proposal(proposal-id)`: Get details about a specific proposal
- `get-member-status(address)`: Check if an address is a member
- `get-dao-treasury()`: View the current treasury balance
- `get-member-contribution(address)`: See how much a member has contributed
- `get-vote(proposal-id, voter)`: Check how someone voted on a proposal
- `is-member(address)`: Verify membership status

## 🚀 Getting Started

1. **Deploy the Contract**: Use Clarinet to deploy the contract to the Stacks blockchain
2. **Join the DAO**: Call `join-dao()` with the membership fee in STX
3. **Contribute Funds**: Add to the treasury with `contribute-funds(amount)`
4. **Create Proposals**: Suggest investments with `create-proposal()`
5. **Vote**: Participate in governance by voting on active proposals
6. **Execute**: After the voting period, execute successful proposals to release funds

## ⚠️ Requirements

- Stacks wallet with STX tokens
- Basic understanding of blockchain transactions

## 🔒 Security Considerations

- All funds are held in the contract
- Proposals require majority approval (default: 51%)
- Only members can create proposals and vote
- Proposal creators can cancel their proposals before execution

Happy community investing! 🌱
```