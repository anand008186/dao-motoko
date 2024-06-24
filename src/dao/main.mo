import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Buffer "mo:base/Buffer";
import Bool "mo:base/Bool";
import Array "mo:base/Array";
import Debug "mo:base/Debug";
import Types "types";
actor {

type Result<A, B> = Result.Result<A, B>;
type Member = Types.Member;
type ProposalContent = Types.ProposalContent;
type ProposalId = Types.ProposalId;
type Proposal = Types.Proposal;
type Vote = Types.Vote;
type HttpRequest = Types.HttpRequest;
type HttpResponse = Types.HttpResponse;

let tokenCanister = actor ("3xyym-nqaaa-aaaab-qacnq-cai") : actor {
        burn : shared (owner : Principal, amount : Nat) -> async Result<(), Text>;
        mint : shared (owner : Principal, amount : Nat) -> async Result<(), Text>;
        balanceOf : shared (owner : Principal)  -> async Nat ;
};

// The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
stable let canisterIdWebpage : Principal = Principal.fromText("3m5ej-xiaaa-aaaab-qacpa-cai");
stable var manifesto = "Your manifesto";
stable let name = "Your DAO";
stable var goals = [];

// Returns the name of the DAO
public query func getName() : async Text {
        return name;
};

// Returns the manifesto of the DAO
public query func getManifesto() : async Text {
        return manifesto;
};

// Returns the goals of the DAO
public query func getGoals() : async [Text] {
        return goals;
};

// Register a new member in the DAO with the given name and principal of the caller
// Airdrop 10 MBC tokens to the new member
// New members are always Student
// Returns an error if the member already exists
let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
// add one mentor to the DAO
members.put(Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai"), { name = "motoko_bootcamp"; role = #Mentor });
// members.put(Principal.fromText("2vxsx-fae"), { name = "Anand"; role = #Mentor });


public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
                case (null) {
                        members.put(caller, { name = member.name; role = #Student });
                        //minting using the token canister
                        await tokenCanister.mint(caller, 10);
                };
                case (?member) {
                        return #err("Member already exists");
                };
        };
};

// Get the member with the given principal
// Returns an error if the member does not exist
public shared query ({ caller }) func getMember(p : Principal) : async Result<Member, Text> {
      Debug.print(debug_show(caller));
        switch (members.get(p)) {
                case (null) {
                        return #err("Member does not exist");
                };
                case (?member) {
                        return #ok(member);
                };
        };
};

// Graduate the student with the given principal
// Returns an error if the student does not exist or is not a student
// Returns an error if the caller is not a mentor
public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
        //check if the caller is a mentor
        switch (members.get(caller)) {
                case (null) {
                        return #err("Caller is not a mentor");
                };
                case (?member) {
                        if (member.role != #Mentor) {
                                return #err("Caller is not a mentor");
                        };
                };
        };
        //check if the student exists
        switch (members.get(student)) {
                case (null) {
                        return #err("Student does not exist");
                };
                case (?member) {
                        switch (member.role) {
                                case (#Student) {
                                        members.put(student, { name = member.name; role = #Graduate });
                                        return #ok(());
                                };
                                case (#Mentor) {
                                        return #err("Mentor cannot be graduated");
                                };
                                case (#Graduate) {
                                        return #err("Student is already graduated");
                                };
                        };

                };
        };
};

// Create a new proposal and returns its id
// Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token

let proposals = Buffer.Buffer<Proposal>(10);
var nextProposalId : ProposalId = 0;
public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch (members.get(caller)) {
                case (null) {
                        return #err("Caller is not a mentor");
                };
                case (?member) {
                        if (member.role != #Mentor) {
                                return #err("Caller is not a mentor");
                        };
                        let balance = await tokenCanister.balanceOf(caller);
                        if (balance < 1) {
                                return #err("Not enough MBC tokens");
                        };

                        //if content is AddMentor check if member exists and is a graduate
                        switch (content) {
                                case (#AddMentor(principal)) {
                                        switch (members.get(principal)) {
                                                case (null) {
                                                        return #err("Member does not exist");
                                                };
                                                case (?member) {
                                                        if (member.role != #Graduate) {
                                                                return #err("Member is not a graduate");
                                                        };
                                                };
                                        };
                                };
                                case (_) {

                                };
                        };

                        //burn 1 MBC token
                        switch (await tokenCanister.burn(caller, 1)) {
                                case (#ok(())) {

                                        let proposal : Proposal = {
                                                id = nextProposalId;
                                                content = content;
                                                creator = caller;
                                                created = Time.now();
                                                executed = null;
                                                votes = [];
                                                voteScore = 0;
                                                status = #Open;
                                        };

                                       proposals.add(proposal);
                                        nextProposalId += 1;
                                        return #ok(proposal.id);

                                };
                                case (#err(err)) {
                                        return #err(err);
                                };
                        };

                };
        };

};

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch (proposals.getOpt(id)) {
                        case (null) {
                                return #err("Proposal does not exist");
                        };
                        case (?proposal) {
                                return #ok(proposal);
                        };
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Buffer.toArray<Proposal>(proposals);
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (null) {
                                return #err("Member does not exist");
                        };
                        case (?member) {
                                if (member.role == #Student) {
                                        return #err("Student is not allowed to vote");
                                };
                                switch (proposals.getOpt(proposalId)) {
                                        case (null) {
                                                return #err("Proposal does not exist");
                                        };
                                        case (?proposal) {
                                                if (proposal.status != #Open) {
                                                        return #err("Proposal is not open");
                                                };
                                                if (_hasVoted(proposal.votes, caller)) {
                                                        return #err("Member already voted");
                                                };
                                                let balance = await tokenCanister.balanceOf(caller);
                                                let voteMultiplier = if (member.role == #Mentor) 5 else 1;
                                                let newVoteScore : Int = proposal.voteScore + balance * voteMultiplier * (if (yesOrNo) 1 else -1);
                                                let newStatus = if (newVoteScore >= 100) #Accepted else if (newVoteScore <= -100) #Rejected else #Open;
                                                var executed : ?Time.Time = null;
                                                if (newStatus == #Accepted) {
                                                        _exectuteProposal(proposal.content);
                                                        executed := ?Time.now();
                                                };

                                                let vote : Vote = {
                                                        member = caller;
                                                        votingPower = balance * voteMultiplier;
                                                        yesOrNo = yesOrNo;
                                                };
                                                let buffer = Buffer.fromArray<Vote>(proposal.votes);
                                                buffer.add(vote);
                                                let newVotes = Buffer.toArray<Vote>(buffer);

                                                let newProposal : Proposal = {
                                                        id = proposal.id;
                                                        content = proposal.content;
                                                        creator = proposal.creator;
                                                        created = proposal.created;
                                                        executed = executed;
                                                        votes = newVotes;
                                                        voteScore = newVoteScore;
                                                        status = newStatus;
                                                };
                                                proposals.put(proposalId, newProposal);
                                                return #ok(());
                                        };
                                };
                        };
                };
        };

        func _hasVoted(votes : [Vote], member : Principal) : Bool {
                return (
                        Array.find<Vote>(
                                votes,
                                func(vote : Vote) : Bool {
                                        return vote.member == member;
                                },
                        ) != null
                );

        };

        func _exectuteProposal(content : ProposalContent) : () {
                switch (content) {
                        case (#ChangeManifesto(newManifesto)) {
                                manifesto := newManifesto;
                        };
                        case (#AddMentor(principal)) {
                                switch (members.get(principal)) {
                                        case (null) {
                                                return;
                                        };
                                        case (?member) {
                                                members.put(principal, { name = member.name; role = #Mentor });
                                        };
                                };
                        };
                        case (_) {
                                return;
                        };
                };
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
