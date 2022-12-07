package de.zogan.aoc2022;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import de.zogan.aoc2022.Day07.Directory;
import de.zogan.aoc2022.Day07.File;

class Day07Tests {

    private static ArrayList<String> lines = new ArrayList<String>(List.of(
            "$ cd /",
            "$ ls",
            "dir a",
            "8504156 c.dat",
            "14848514 b.txt",
            "dir d",
            "$ cd a",
            "$ ls",
            "dir e",
            "29116 f",
            "2557 g",
            "62596 h.lst",
            "$ cd e",
            "$ ls",
            "584 i",
            "$ cd ..",
            "$ cd ..",
            "$ cd d",
            "$ ls",
            "4060174 j",
            "8033020 d.log",
            "5626152 d.ext",
            "7214296 k"));

    @Test
    void parseTerminalOutput_FillsFileSystem() {
        var root = new Directory();

        Day07.parseTerminalOutput(lines, root);

        // /
        assertSubDirs(Set.of("d", "a"), root.subDirs);
        var expectedRootDirFiles = Set.of(
                new File("c.dat", 8504156),
                new File("b.txt", 14848514));
        assertFiles(expectedRootDirFiles, root.files);
        assertEquals(48381165, root.getSize());

        // /a/
        var subDirA = root.subDirs.get("a");
        assertSubDirs(Set.of("e"), subDirA.subDirs);
        var expectedDirAFiles = Set.of(
                new File("f", 29116),
                new File("g", 2557),
                new File("h.lst", 62596));
        assertFiles(expectedDirAFiles, subDirA.files);
        assertEquals(94853, subDirA.getSize());

        // /a/e/
        var subDirE = subDirA.subDirs.get("e");
        assertSubDirs(Set.of(), subDirE.subDirs);
        assertFiles(Set.of(new File("i", 584)), subDirE.files);
        assertEquals(584, subDirE.getSize());

        // /d/
        var subDirD = root.subDirs.get("d");
        assertSubDirs(Set.of(), subDirD.subDirs);
        var expectedDirDFiles = Set.of(
                new File("j", 4060174),
                new File("d.log", 8033020),
                new File("d.ext", 5626152),
                new File("k", 7214296));
        assertFiles(expectedDirDFiles, subDirD.files);
        assertEquals(24933642, subDirD.getSize());
    }

    @Test
    void findDirectories_HaveCorrectSize() {
        var root = new Directory();
        Day07.parseTerminalOutput(lines, root);

        var directories = root.findDirectories(100000);
        assertDirs(Set.of("a", "e"), Set.of(94853, 584), directories);
    }

    @Test
    void getTotalSize_ReturnsCorrectSize() {
        var root = new Directory();
        Day07.parseTerminalOutput(lines, root);

        assertEquals(95437, Day07.getTotalSize(root));
    }

    private void assertDirs(
            Set<String> expectedNames,
            Set<Integer> expectedSizes,
            Set<Directory> actual) {
        var actualNames = actual.stream()
                .map(Directory::getName)
                .collect(Collectors.toSet());
        assertEquals(expectedNames, actualNames);

        var actualSizes = actual.stream()
                .map(Directory::getSize)
                .collect(Collectors.toSet());
        assertEquals(expectedSizes, actualSizes);
    }

    private void assertSubDirs(Set<String> expectedDirNames,
            HashMap<String, Directory> actual) {
        assertEquals(expectedDirNames, actual.keySet());
    }

    private void assertFiles(Set<File> expected, Set<File> actual) {
        var expectedFileNames = expected.stream()
                .map(File::getFilename)
                .collect(Collectors.toSet());
        var actualFileNames = actual.stream()
                .map(File::getFilename)
                .collect(Collectors.toSet());
        assertEquals(expectedFileNames, actualFileNames);

        var expectedFileSizes = expected.stream()
                .map(File::getSize)
                .collect(Collectors.toSet());
        var actualFileSizes = actual.stream()
                .map(File::getSize)
                .collect(Collectors.toSet());
        assertEquals(expectedFileSizes, actualFileSizes);
    }
}
