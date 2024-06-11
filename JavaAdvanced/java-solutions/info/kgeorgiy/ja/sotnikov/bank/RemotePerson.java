package info.kgeorgiy.ja.sotnikov.bank;

import java.rmi.RemoteException;

/**
 * A {@link Person} implementation that interacts with a remote bank.
 *
 * @see AbstractPerson
 * @see Person
 */
public class RemotePerson extends AbstractPerson {

    public RemotePerson(String name, String surname, String passportId, Bank bank) {
        super(name, surname, passportId, bank);
    }

    /**
     * Creates a new account in the bank associated with the specified subId for this person.
     *
     * @param subId the sub-identifier of an account.
     * @return the newly created account.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    @Override
    public Account createAccountImpl(String subId) throws RemoteException {
        return bank.createAccount(this, subId);
    }

    /**
     * Retrieves the account from the bank associated with the specified subId for this person.
     *
     * @param subId the sub-identifier of an account.
     * @return the account associated with the specified subId.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    @Override
    public Account getAccountImpl(String subId) throws RemoteException {
        return bank.getAccount(this, subId);
    }
}
