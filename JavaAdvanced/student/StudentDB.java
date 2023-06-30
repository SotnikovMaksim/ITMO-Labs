package info.kgeorgiy.ja.sotnikov.student;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import info.kgeorgiy.java.advanced.student.StudentQuery;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.GroupName;

public class StudentDB implements StudentQuery {

    protected static <R> List<R> mapStudents(final List<Student> students, final Function<Student, R> mapper) {
        return students
                .stream()
                .map(mapper)
                .toList();
    }

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return mapStudents(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return mapStudents(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        return mapStudents(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return mapStudents(students, student -> student.getFirstName() + " " + student.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return students
                .stream()
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        return students
                .stream()
                .max(Comparator.naturalOrder())
                .map(Student::getFirstName)
                .orElse("");
    }

    protected List<Student> sortStudentsByComparator(final Collection<Student> students,
                                                     final Comparator<Student> comparator)
    {
        return students
                .stream()
                .sorted(comparator)
                .toList();
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return sortStudentsByComparator(students, Comparator.naturalOrder());
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return sortStudentsByComparator(students,
                Comparator.comparing(Student::getLastName)
                        .thenComparing(Student::getFirstName).reversed()
                        .thenComparing(Student::getId)
        );
    }


    protected <R> List<Student> filterStudentsBy(final R equiv, final Collection<Student> students,
                                                 final Function<Student, R> function) {
        return sortStudentsByName(students)
                .stream()
                .filter(s -> function.apply(s).equals(equiv))
                .toList();
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, final String name) {
        return filterStudentsBy(name, students, Student::getFirstName);
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, final String name) {
        return filterStudentsBy(name, students, Student::getLastName);
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students, final GroupName group) {
        return filterStudentsBy(group, students, Student::getGroup);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, final GroupName group) {
        return filterStudentsBy(group, students, Student::getGroup)
                .stream()
                .sorted(Comparator.comparing(Student::getFirstName).reversed())
                .collect(Collectors.toMap(Student::getLastName,
                        Student::getFirstName,
                        (x, y) -> y,
                        LinkedHashMap::new)
                );
    }
}
