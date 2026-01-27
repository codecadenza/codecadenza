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
package net.codecadenza.eclipse.tools.model;

import static net.codecadenza.eclipse.shared.Constants.MODEL_FILE_EXTENSION;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

/**
 * <p>
 * Simple application to either enhance or to repair existing meta-models
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ModelRepairTool {
	private static final String PROJECT_FILE_SUFFIX = ".zip";

	// Define a filter to get *.xmi files only
	private static final FilenameFilter FILTER = (_, name) -> name.endsWith("." + MODEL_FILE_EXTENSION);

	/**
	 * @param args
	 * @throws Exception if the model repair operation has failed
	 */
	public static final void main(String[] args) throws Exception {
		Enumeration<? extends ZipEntry> entries;
		var inputFolder = "";

		if (args.length == 1)
			inputFolder = args[0];

		// Iterate over all project files to be changed!
		for (final String projectFileName : new File(inputFolder).list()) {
			try (final var projectZipFile = new ZipFile(inputFolder + projectFileName)) {
				entries = projectZipFile.entries();

				// Extract all meta-model files
				while (entries.hasMoreElements()) {
					final ZipEntry entry = entries.nextElement();

					// We don't expect a directory!
					if (entry.isDirectory())
						continue;

					copyInputStream(projectZipFile.getInputStream(entry),
							new BufferedOutputStream(new FileOutputStream(inputFolder + entry.getName())));
				}
			}

			// Open respective files to be repaired
			for (final String modelFileName : new File(inputFolder).list(FILTER)) {
				final String outputFileName = inputFolder + modelFileName;
				final var fileContent = new StringBuilder();

				// Read the file line by line
				try (final var reader = new BufferedReader(new FileReader(outputFileName))) {
					String line = null;

					while (true) {
						line = reader.readLine();

						if (line == null)
							break;

						// In this area we can change a model line by line!
						fileContent.append(line);
						fileContent.append("\n");
					}
				}

				// Overwrite the existing file!
				try (final var writer = new BufferedWriter(new FileWriter(outputFileName))) {
					writer.append(fileContent);
				}
			}

			// Delete the existing project file
			Files.delete(new File(inputFolder + projectFileName).toPath());

			// Recreate the project file
			try (final var zipOutputStream = new ZipOutputStream(new FileOutputStream(inputFolder + projectFileName))) {
				for (final String modelFileName : new File(inputFolder).list()) {
					if (modelFileName.endsWith(PROJECT_FILE_SUFFIX))
						continue;

					addDocumentToZipFile(zipOutputStream, inputFolder + modelFileName, modelFileName);

					// Delete the meta-model file
					Files.delete(new File(inputFolder + modelFileName).toPath());
				}
			}
		}
	}

	/**
	 * Add the document to the zip file
	 * @param zipOutputStream
	 * @param path
	 * @param fileName
	 * @throws Exception if the zip file either could not be found, or the document could not be added
	 */
	private static void addDocumentToZipFile(ZipOutputStream zipOutputStream, String path, String fileName) throws Exception {
		// Create a buffer for reading file data
		final var buf = new byte[1024];

		try (final var in = new FileInputStream(path)) {
			// Add a new entry to the output stream
			zipOutputStream.putNextEntry(new ZipEntry(fileName));

			// Transfer the content from the source file to the compressed file
			int len;

			while ((len = in.read(buf)) > 0)
				zipOutputStream.write(buf, 0, len);

			// Complete the entry
			zipOutputStream.closeEntry();
		}
	}

	/**
	 * Copy the content of the provided input stream into the output stream
	 * @param in the stream where the data is read from
	 * @param out the stream where the data is written to
	 * @throws IOException if the copy operation has failed
	 */
	private static final void copyInputStream(InputStream in, OutputStream out) throws IOException {
		final var buffer = new byte[1024];
		int len;

		try {
			while ((len = in.read(buffer)) >= 0)
				out.write(buffer, 0, len);
		}
		finally {
			in.close();
			out.close();
		}
	}

}
