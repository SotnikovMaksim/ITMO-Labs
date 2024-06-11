package info.kgeorgiy.ja.sotnikov.bank;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.lang.reflect.Constructor;
import java.net.MalformedURLException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.IntStream;

//@FixMethodOrder(MethodSorters.NAME_ASCENDING)
@RunWith(JUnit4.class)
public class BankTests {

    private static Registry RMI;
    private static final String BANK_URL = "//localhost/bank";
    private static final String CLIENT_CLASS_NAME = "info.kgeorgiy.ja.sotnikov.bank.Client";
    private static final String ACCOUNT_SUB_ID = "0";
    private static final int AMOUNT_DELTA = 100;
    private static final int BANK_PORT = 8881;
    private static final int RMI_REGISTRY_PORT = 8882;
    private static final int CLIENTS_COUNT = 5;
    private static final Client[] CLIENTS = new Client[]{
            new Client("Айнур", "Абрамов", "0"),
            new Client("Александр", "Алентьев", "1"),
            new Client("Александра", "Андосова", "2"),
            new Client("Алексей", "Бактурин", "3"),
            new Client("Алиссия", "Бандурина", "4"),
    };

    public BankTests() {
    }

    @BeforeClass
    public static void beforeClass() {
        try {
            close();
        } catch (final RemoteException | MalformedURLException | NotBoundException ignored) {
        }
    }

    @Rule
    public ExpectedException expectedException = ExpectedException.none();

    @Rule
    public final TestRule watcher = new TestWatcher() {
        @Override
        protected void starting(final Description description) {
            try {
                init();
            } catch (final RemoteException e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        protected void finished(final Description description) {
            try {
                close();
            } catch (final RemoteException | MalformedURLException | NotBoundException e) {
                throw new RuntimeException(e);
            }
        }
    };


    @Test
    public void test01_person_creation() throws RemoteException, NotBoundException {
        Person person;
        Bank bank = createBank();

        for (int i = 0; i < CLIENTS_COUNT; i++) {
            person = bank.getRemotePerson(Integer.toString(i));
            Assert.assertNull(person);
        }

        unbindBank(bank);
    }

    @Test
    public void test02_existing_person() throws RemoteException, NotBoundException {
        Person person;
        Person expectedPerson;
        Bank bank = createBank();

        for (int i = 0; i < CLIENTS_COUNT; i++) {
            bank.createPerson(CLIENTS[i].name, CLIENTS[i].surname, CLIENTS[i].passportId);
            person = bank.getRemotePerson(Integer.toString(i));
            expectedPerson = new RemotePerson(CLIENTS[i].name, CLIENTS[i].surname, CLIENTS[i].passportId, bank);

            assertEqualsPersons(person, expectedPerson);
        }

        unbindBank(bank);
    }

    @Test
    public void test03_wrong_data() throws Exception {
        Bank bank = createBank();
        Constructor<?> clientConstructor = getClientConstructor();

        for (int i = 1; i < CLIENTS_COUNT; i++) {
            goToBank(CLIENTS[i].name, CLIENTS[i].surname, CLIENTS[i].passportId, bank, clientConstructor);

            expectedException.expect(IllegalArgumentException.class);

            goToBank(CLIENTS[(i + 1) % CLIENTS_COUNT].name, CLIENTS[i].surname, CLIENTS[i].passportId, bank, clientConstructor);
        }

        unbindBank(bank);
    }

    @Test
    public void test04_parallelClients() throws ClassNotFoundException, NoSuchMethodException, RemoteException, NotBoundException {
        final int THREADS_COUNT = 10;
        final int LOOP_COUNT = 5;

        final Constructor<?> clientConstructor = getClientConstructor();
        final ExecutorService pool = Executors.newFixedThreadPool(THREADS_COUNT);
        final Bank bank = createBank();

        IntStream.range(0, THREADS_COUNT)
                .mapToObj(i -> pool.submit(() -> {
                    for (int j = 0; j < CLIENTS_COUNT * LOOP_COUNT; j++) {
                        try {
                            goToBank(CLIENTS[j % CLIENTS_COUNT].name,
                                    CLIENTS[j % CLIENTS_COUNT].surname,
                                    CLIENTS[j % CLIENTS_COUNT].passportId,
                                    bank, clientConstructor
                            );
                        } catch (Exception ignored) {
                        }
                    }
                }))
                .forEach(future -> {
                    while (true) {
                        try {
                            future.get();
                            break;
                        } catch (InterruptedException | ExecutionException ignored) {
                        }
                    }
                }
        );

        for (int i = 0; i < CLIENTS_COUNT; i++) {
            Assert.assertEquals(bank.getAccount(bank.getRemotePerson(CLIENTS[i].passportId), ACCOUNT_SUB_ID).getAmount(),
                    AMOUNT_DELTA * LOOP_COUNT * THREADS_COUNT);
        }

        unbindBank(bank);
    }

    @Test
    public void test05_local_account_creation() throws RemoteException {
        accountsCreation(bank -> new LocalPerson("Robert", "Martin", "0", bank, new HashMap<>()));
    }

    @Test
    public void test06_remote_account_creation() throws RemoteException {
        accountsCreation(bank -> new RemotePerson("Robert", "Martin", "0", bank));
    }

    private static void accountsCreation(Function<Bank, Person> gersonGetter) throws RemoteException {
        final Bank bank = createBank();
        final int ACCOUNTS_COUNT = 10;
        final Person person = gersonGetter.apply(bank);

        for (int i = 0; i < ACCOUNTS_COUNT; i++) {
            person.createAccount(Integer.toString(i));
            person.getAccount(Integer.toString(i));
        }
    }

    private void goToBank(String name, String surname, String passportId, Bank bank, Constructor<?> clientConstructor) throws Exception {
        ((info.kgeorgiy.ja.sotnikov.bank.Client)
                clientConstructor.newInstance()
        ).goToBank(
                name,
                surname,
                passportId,
                ACCOUNT_SUB_ID,
                AMOUNT_DELTA,
                bank
        );
    }

    private Constructor<?> getClientConstructor() throws ClassNotFoundException, NoSuchMethodException {
        return Class.forName(CLIENT_CLASS_NAME).getConstructor();
    }

    private static void assertEqualsPersons(Person person, Person expectedPerson) throws RemoteException {
        String expectedName = expectedPerson.getName(), actualName = person.getName();
        String expectedSurname = expectedPerson.getSurname(), actualSurname = person.getSurname();
        String expectedPassportId = expectedPerson.getPassportId(), actualPassportId = person.getPassportId();
        Assert.assertEquals("Wrong name! Expected %s, but was %s".formatted(expectedName, actualName), expectedName, actualName);
        Assert.assertEquals("Wrong surname! Expected %s, but was %s".formatted(expectedSurname, actualSurname), expectedSurname, actualSurname);
        Assert.assertEquals("Wrong passport! Expected %s, but was %s".formatted(expectedPassportId, actualPassportId), expectedPassportId, actualPassportId);
    }

    protected static void init() throws RemoteException {
        RMI = LocateRegistry.createRegistry(RMI_REGISTRY_PORT);
    }

    private static Bank createBank() {
        Bank bank = new RemoteBank(BANK_PORT);
        bindBank(bank);

        return bank;
    }

    private static void bindBank(final Bank bank) {
        try {
            UnicastRemoteObject.exportObject(bank, BANK_PORT);
            RMI.rebind(BANK_URL, bank);
        } catch (final RemoteException e) {
            throw new RuntimeException(e);
        }
    }

    private static void unbindBank(Bank bank) throws RemoteException, NotBoundException {
        UnicastRemoteObject.unexportObject(bank, true);
        RMI.unbind(BANK_URL);
    }

    protected static void close() throws RemoteException, MalformedURLException, NotBoundException {
        UnicastRemoteObject.unexportObject(RMI, true);
    }

    private record Client(String name, String surname, String passportId) {
    }
}

