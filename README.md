# 🎯 Donatix - Charity Milestone Escrow

> 💝 Release donations based on real progress - Building trust in charitable giving through milestone-based funding

## 🌟 Overview

Donatix is a revolutionary charity platform built on the Stacks blockchain that ensures donations are released only when charities demonstrate real progress. By implementing milestone-based escrow, donors can trust that their contributions will be used effectively and transparently.

## ✨ Key Features

- 🏗️ **Campaign Creation**: Charities can create fundraising campaigns with clear goals
- 💰 **Secure Donations**: Funds are held in escrow until milestones are achieved  
- 🎯 **Milestone Tracking**: Break down projects into verifiable milestones
- 🗳️ **Community Voting**: Donors vote on milestone completion
- 🔒 **Trustless Release**: Funds automatically release when milestones pass community approval
- 📊 **Full Transparency**: All activities are recorded on-chain

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd donatix
clarinet check
```

## 📖 Usage Guide

### For Charities 🏥

1. **Create Campaign**
   ```clarity
   (contract-call? .Donatix create-campaign "Clean Water Project" "Providing clean water to rural communities" u100000)
   ```

2. **Add Milestones**
   ```clarity
   (contract-call? .Donatix create-milestone u1 "Well Construction" "Complete first water well" u25000 u144)
   ```

3. **Submit Completion**
   ```clarity
   (contract-call? .Donatix submit-milestone-completion u1)
   ```

### For Donors 💝

1. **Make Donation**
   ```clarity
   (contract-call? .Donatix donate u1 u1000)
   ```

2. **Vote on Milestones**
   ```clarity
   (contract-call? .Donatix vote-on-milestone u1 true)
   ```

### For Anyone 👥

1. **View Campaign**
   ```clarity
   (contract-call? .Donatix get-campaign u1)
   ```

2. **Check Milestone Status**
   ```clarity
   (contract-call? .Donatix get-milestone u1)
   ```

## 🔧 Core Functions

### Public Functions

| Function | Description | Access |
|----------|-------------|---------|
| `create-campaign` | Create new fundraising campaign | Anyone |
| `donate` | Contribute to a campaign | Anyone |
| `create-milestone` | Add milestone to campaign | Campaign Creator |
| `submit-milestone-completion` | Mark milestone as completed | Campaign Creator |
| `vote-on-milestone` | Vote on milestone completion | Donors Only |
| `release-milestone-funds` | Release approved milestone funds | Anyone |
| `close-campaign` | Deactivate campaign | Campaign Creator |

### Read-Only Functions

- `get-campaign` - Retrieve campaign details
- `get-milestone` - Get milestone information  
- `get-donation` - Check donation amount
- `get-milestone-vote` - View voting record
- `get-escrow-balance` - Check held funds

## 🛡️ Security Features

- ✅ **Access Control**: Only authorized users can perform sensitive operations
- ✅ **Input Validation**: All inputs are validated before processing
- ✅ **Escrow Protection**: Funds are safely held until milestone approval
- ✅ **Voting Rights**: Only donors can vote on milestone completion
- ✅ **Double-Spend Prevention**: Robust checks prevent fund manipulation

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

We welcome contributions! Please feel free to submit pull requests or open issues for bugs and feature requests.

## 📄 License

This project is open source and available under the MIT License.

## 🌍 Impact

Donatix aims to revolutionize charitable giving by:
- 📈 Increasing donor confidence through transparency
- 🎯 Ensuring funds reach intended beneficiaries  
- 📊 Providing real-time progress tracking
- 🤝 Building stronger charity-donor relationships
- 🌱 Enabling sustainable long-term projects

---

*Built on Stacks blockchain for a more transparent world* 🌟