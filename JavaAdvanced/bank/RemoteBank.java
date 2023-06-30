package info.kgeorgiy.ja.sotnikov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Supplier;

/**
 * Implementation of {@link Bank} interface that provides remote access to the bank service.
 * This class manages {@link Person} and {@link Account} entities.
 * Thread-safety is achieved by using concurrent collections.
 *
 * @see Bank
 * @see Person
 * @see Account
 */
public class RemoteBank implements Bank {
    /**
     * Port for remote connections.
     */
    private final int port;
    /**
     * Concurrent map of persons in the bank.
     * Key - passport ID, Value - {@link Person} object.
     */
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();
    /**
     * Concurrent map of accounts in the bank.
     * Key - (passportId:subId), Value - {@link Account} object.
     */
    private final ConcurrentMap<String, ConcurrentMap<String, Account>> accounts = new ConcurrentHashMap<>();

    /**
     * Constructs a {@link RemoteBank} object with the specified port for remote connections.
     *
     * @param port the port to be used for remote connections.
     */
    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Person createPerson(final String name, final String surname, final String passportId) throws RemoteException {
        return createEntity(
                passportId,
                persons,
                () -> new RemotePerson(name, surname, passportId, this),
                () -> getRemotePerson(passportId)
        );
    }

    @Override
    public Account createAccount(Person person, String subId) throws RemoteException {
        checkPersonAccountsExistence(person);

        return createEntity(
                subId,
                accounts.get(person.getPassportId()),
                () -> new RemoteAccount(subId),
                () -> getAccount(person, subId)
        );
    }

    private <T extends Remote> T createEntity(String key, Map<String, T> map,
                                              Supplier<T> constructor,
                                              BankUtilities.ThrowingSupplier<T> getter
    ) throws RemoteException {
        if (!map.containsKey(key)) {
            T entity = constructor.get();
            map.put(key, entity);
            UnicastRemoteObject.exportObject(entity, port);
            return entity;
        } else {
            return getter.get();
        }
    }

    @Override
    public Person getRemotePerson(final String passportId) throws RemoteException {
        return get(passportId,
                "Bank does not have registered person with such subId",
                persons);
    }

    @Override
    public Account getAccount(Person person, String subId) throws RemoteException {
        checkPersonAccountsExistence(person);

        return get(
                subId,
                "Bank does not have registered account with such subId",
                accounts.get(person.getPassportId())
        );
    }

    private void checkPersonAccountsExistence(Person person) throws RemoteException {
        if (accounts.putIfAbsent(person.getPassportId(), new ConcurrentHashMap<>()) == null) {
            System.out.println("Person doesn't have accounts before");
        }
    }

    private static <T> T get(String key, String messageIfAbsent, Map<String, T> map) {
        T value = map.get(key);

        if (Objects.isNull(value)) {
            System.out.println(messageIfAbsent);
        }

        return value;
    }

    @Override
    public Person getLocalPerson(final String passportId) throws RemoteException {
        Person person = persons.get(passportId);

        if (Objects.isNull(person)) {
            System.out.println("Person with such passport ID does not exist");
            return null;
        }

        Map<String, Account> map = accounts.get(passportId);
        Map<String, Account> localAccounts = new HashMap<>();

        if (!Objects.isNull(map)) {
            for (final Map.Entry<String, Account> accountPair: map.entrySet()) {
                localAccounts.put(
                        accountPair.getKey(),
                        new LocalAccount(accountPair.getValue().getId())
                );
            }
        }

        return new LocalPerson(
                person.getName(),
                person.getSurname(),
                passportId,
                null,
                localAccounts
        );
    }
}
