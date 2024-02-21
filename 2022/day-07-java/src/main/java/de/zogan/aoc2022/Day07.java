package de.zogan.aoc2022;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class Day07 {

    static class File {
        private String filename;

        public String getFilename() {
            return filename;
        }

        private int size;

        public int getSize() {
            return size;
        }

        public File(String filename, int size) {
            this.filename = filename;
            this.size = size;
        }
    }

    static class Directory {

        private String name;

        public String getName() {
            return name;
        }

        private Directory parent;

        public Directory getParent() {
            return parent;
        }

        public Directory() {
            this("/", null);
        }

        public Directory(String name, Directory parent) {
            this.name = name;
            this.parent = parent;
        }

        public HashMap<String, Directory> subDirs = new HashMap<String, Directory>();
        public Set<File> files = new HashSet<File>();

        private int size = -1;

        public int getSize() {
            if (size == -1) {
                size = files.stream().mapToInt(File::getSize).sum() +
                        subDirs.values().stream().mapToInt(Directory::getSize).sum();
            }
            return size;
        }

        public Set<Directory> findDirectoriesWithMinSize(int minimumSize) {
            var dirs = new HashSet<Directory>();
            if (getSize() >= minimumSize) {
                dirs.add(this);
            }
            for (var subDir : subDirs.values()) {
                dirs.addAll(subDir.findDirectoriesWithMinSize(minimumSize));
            }
            return dirs;
        }

        public Set<Directory> findDirectoriesWithMaxSize(int maximumSize) {
            var dirs = new HashSet<Directory>();
            if (getSize() <= maximumSize) {
                dirs.add(this);
            }
            for (var subDir : subDirs.values()) {
                dirs.addAll(subDir.findDirectoriesWithMaxSize(maximumSize));
            }
            return dirs;
        }
    }

    public static int getTotalSize(Directory root) {
        return root.findDirectoriesWithMaxSize(100000)
                .stream()
                .map(Directory::getSize)
                .mapToInt(Integer::intValue)
                .sum();
    }

    public static int getSpaceToBeDeleted(Directory root) {
        var totalDiskSpace = 70000000;
        var requiredDiskSpace = 30000000;

        var freeSpace = totalDiskSpace - root.getSize();
        return requiredDiskSpace - freeSpace;
    }

    public static int sizeOfSmallestDirToDelete(Directory root) {
        var minimumDirSize = getSpaceToBeDeleted(root);
        return root.findDirectoriesWithMinSize(minimumDirSize)
                .stream()
                .map(Directory::getSize)
                .mapToInt(Integer::intValue)
                .sorted()
                .findFirst()
                .getAsInt();
    }

    public static void main(String[] args) {
        var lines = readFile("day07.in");

        var root = new Directory();
        parseTerminalOutput(lines, root);
        var totalSize = getTotalSize(root);

        var dirToDeleteSize = sizeOfSmallestDirToDelete(root);

        System.out.println("Part 1: " + totalSize);
        System.out.println("Part 2: " + dirToDeleteSize);
    }

    public static void parseTerminalOutput(ArrayList<String> lines, Directory dir) {
        var currentDir = dir;

        for (var line : lines) {
            if (line.equals("$ cd /") || line.equals("$ ls")) {
                continue;
            }
            if (line.startsWith("$ cd")) {
                var parts = line.split(" ");
                var dirname = parts[2];
                if (dirname.equals("..")) {
                    currentDir = currentDir.parent;
                } else {
                    currentDir = currentDir.subDirs.get(dirname);
                }
            } else {
                // ls output
                var parts = line.split(" ");
                if (parts[0].equals("dir")) {
                    var dirname = parts[1];
                    currentDir.subDirs.put(dirname, new Directory(dirname, currentDir));
                } else {
                    var filename = parts[1];
                    var size = Integer.parseInt(parts[0]);
                    currentDir.files.add(new File(filename, size));
                }
            }
        }
    }

    private static ArrayList<String> readFile(String filename) {
        var lines = new ArrayList<String>();
        try (var reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
        return lines;
    }
}
