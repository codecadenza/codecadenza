/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.tools.util.version;

import static net.codecadenza.eclipse.shared.Constants.POM_XML;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * Tool for changing the CodeCadenza version numbers in different build and configuration files. It also deletes all obsolete
 * runtime libraries.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VersionUpdateTool {
	private static final String ERROR_MSG = "The tool needs three launch arguments:\n1) The fully-qualified path to the workspace.\n2) The current version number.\n3) The new version number.";
	private static final String PLUGIN_NS = "net.codecadenza.eclipse";
	private static final String RUNTIME_NS = "net.codecadenza.runtime";
	private static final String LIB_FILE_PREFIX = "codecadenza-";
	private static final String MANIFEST_FM = "MANIFEST.MF";
	private static final String FEATURE_XML = "feature.xml";
	private static final String DEPENDENCIES_XML = "dependencies.xml";
	private static final String SITE_XML = "site.xml";
	private static final String BUILD_PROPERTIES = "build.properties";
	private static final String CLASSPATH_FILE = ".classpath";
	private static final String NEW_LINE = System.lineSeparator();

	private static String currentVersion;
	private static String newVersion;

	/**
	 * @param args
	 * @throws IllegalArgumentException if the application has been launched with an unexpected number of arguments
	 * @throws IOException if an I/O error has occurred
	 */
	public static void main(String[] args) throws IOException {
		if (args.length != 3)
			throw new IllegalArgumentException(ERROR_MSG);

		final String rootPath = args[0];
		currentVersion = args[1];
		newVersion = args[2];

		final var rootFolder = new File(rootPath);
		final List<File> files = getFiles(new ArrayList<>(), rootFolder.toPath());

		// Iterate over all files in the workspace
		for (final File file : files) {
			if (!file.isFile())
				continue;

			if (file.getName().equals(POM_XML))
				changePomFile(file);
			else if (file.getName().equals(MANIFEST_FM))
				changeManifestFile(file);
			else if (file.getName().equals(FEATURE_XML))
				changeFeatureFile(file);
			else if (file.getName().equals(DEPENDENCIES_XML))
				changeDependenciesFile(file);
			else if (file.getName().equals(SITE_XML))
				changeSiteFile(file);
			else if (file.getName().equals(CLASSPATH_FILE))
				changeClasspathFile(file);
			else if (file.getName().equals(BUILD_PROPERTIES))
				changeBuildPropertiesFile(file);
			else if (file.getName().startsWith(LIB_FILE_PREFIX) && file.getName().contains(currentVersion))
				Files.delete(file.toPath());
		}
	}

	/**
	 * Change the version in the given pom.xml file
	 * @param pomFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changePomFile(File pomFile) throws IOException {
		final var currentVersionFragment = "<version>" + currentVersion + "</version>";
		final var newVersionFragment = "<version>" + newVersion + "</version>";

		replaceVersionFragment(pomFile, currentVersionFragment, newVersionFragment);
	}

	/**
	 * Change the version in the given MANIFEST.MF file
	 * @param manifestFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changeManifestFile(File manifestFile) throws IOException {
		var currentVersionFragment = "Bundle-Version: " + currentVersion;
		var newVersionFragment = "Bundle-Version: " + newVersion;

		replaceVersionFragment(manifestFile, currentVersionFragment, newVersionFragment);

		currentVersionFragment = "bundle-version=\"" + currentVersion + "\"";
		newVersionFragment = "bundle-version=\"" + newVersion + "\"";

		replaceVersionFragment(manifestFile, currentVersionFragment, newVersionFragment, PLUGIN_NS);

		currentVersionFragment = "-" + currentVersion + ".jar";
		newVersionFragment = "-" + newVersion + ".jar";

		replaceVersionFragment(manifestFile, currentVersionFragment, newVersionFragment, LIB_FILE_PREFIX);
	}

	/**
	 * Change the version in the given feature.xml file
	 * @param featureFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changeFeatureFile(File featureFile) throws IOException {
		final var currentVersionFragment = "version=\"" + currentVersion + "\"";
		final var newVersionFragment = "version=\"" + newVersion + "\"";

		replaceVersionFragment(featureFile, currentVersionFragment, newVersionFragment, PLUGIN_NS);
	}

	/**
	 * Change the version in the given dependencies.xml file
	 * @param dependenciesFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changeDependenciesFile(File dependenciesFile) throws IOException {
		final var currentVersionFragment = "version=\"" + currentVersion + "\"";
		final var newVersionFragment = "version=\"" + newVersion + "\"";

		replaceVersionFragment(dependenciesFile, currentVersionFragment, newVersionFragment, RUNTIME_NS);
	}

	/**
	 * Change the version in the given site.xml file
	 * @param siteFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changeSiteFile(File siteFile) throws IOException {
		replaceVersionFragment(siteFile, currentVersion, newVersion, PLUGIN_NS);
	}

	/**
	 * @param classpathFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changeClasspathFile(File classpathFile) throws IOException {
		final var currentVersionFragment = "-" + currentVersion + ".jar";
		final var newVersionFragment = "-" + newVersion + ".jar";

		replaceVersionFragment(classpathFile, currentVersionFragment, newVersionFragment, LIB_FILE_PREFIX);
	}

	/**
	 * @param buildPropertiesFile
	 * @throws IOException if an I/O error has occurred
	 */
	private static void changeBuildPropertiesFile(File buildPropertiesFile) throws IOException {
		final var currentVersionFragment = "-" + currentVersion + ".jar";
		final var newVersionFragment = "-" + newVersion + ".jar";

		replaceVersionFragment(buildPropertiesFile, currentVersionFragment, newVersionFragment, LIB_FILE_PREFIX);
	}

	/**
	 * Replace all occurrences of <code>currentVersionFragment</code> with <code>newVersionFragment</code> if a line contains a
	 * respective filter criterion
	 * @param file
	 * @param currentVersionFragment
	 * @param newVersionFragment
	 * @param lineFilter
	 * @throws IOException if an I/O error has occurred
	 */
	private static void replaceVersionFragment(File file, String currentVersionFragment, String newVersionFragment,
			String lineFilter) throws IOException {
		final List<String> lines = Files.readAllLines(file.toPath());
		final var b = new StringBuilder();

		lines.forEach(line -> {
			if (lineFilter == null || line.contains(lineFilter))
				b.append(line.replace(currentVersionFragment, newVersionFragment));
			else
				b.append(line);

			b.append(NEW_LINE);
		});

		Files.write(file.toPath(), b.toString().getBytes(), StandardOpenOption.TRUNCATE_EXISTING);
	}

	/**
	 * Replace all occurrences of <code>currentVersionFragment</code> with <code>newVersionFragment</code>
	 * @param file
	 * @param currentVersionFragment
	 * @param newVersionFragment
	 * @throws IOException if an I/O error has occurred
	 */
	private static void replaceVersionFragment(File file, String currentVersionFragment, String newVersionFragment)
			throws IOException {
		replaceVersionFragment(file, currentVersionFragment, newVersionFragment, null);
	}

	/**
	 * Recursive function to collect all files of a given root directory
	 * @param files
	 * @param dir
	 * @return a list containing all files of the given directory
	 * @throws IOException if an I/O error has occurred
	 */
	private static List<File> getFiles(List<File> files, Path dir) throws IOException {
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
			for (final Path path : stream)
				if (path.toFile().isDirectory())
					getFiles(files, path);
				else
					files.add(path.toFile());
		}

		return files;
	}

}
