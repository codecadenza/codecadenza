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
package net.codecadenza.eclipse.tools.util.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.attribute.FileTime;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * <p>
 * Tool for modifying the timestamps of all entries contained in a zip file
 * </p>
 * <p>
 * Copyright 2025 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ZipTimestampModifier {
	private static final int BUFFER_SIZE = 1024;

	/**
	 * @param args
	 * @throws IllegalArgumentException if the application has been launched with an unexpected number of arguments
	 * @throws IOException if an I/O error has occurred
	 */
	public static void main(String[] args) throws IOException {
		if (args.length != 2)
			throw new IllegalArgumentException("The application requires an argument for the input path and the output path!");

		final String inputZipFilePath = args[0];
		final String outputZipFilePath = args[1];
		final FileTime newFileTime = FileTime.fromMillis(System.currentTimeMillis());

		if (!(new File(inputZipFilePath).exists()))
			throw new IllegalArgumentException("The input file could not be found!");

		if (new File(outputZipFilePath).isDirectory())
			throw new IllegalArgumentException("The output path points to a directory!");

		// Read the input zip file
		try (final var zipInputStream = new ZipInputStream(new FileInputStream(inputZipFilePath))) {
			try (final var zipOutputStream = new ZipOutputStream(new FileOutputStream(outputZipFilePath))) {
				ZipEntry existingEntry;

				while ((existingEntry = zipInputStream.getNextEntry()) != null) {
					// Create a new entry and set the timestamps
					final var newEntry = new ZipEntry(existingEntry.getName());
					newEntry.setCreationTime(newFileTime);
					newEntry.setLastModifiedTime(newFileTime);

					// Write the new entry to the output file
					zipOutputStream.putNextEntry(newEntry);

					// Copy the original entry's data to the new entry
					final byte[] buffer = new byte[BUFFER_SIZE];
					int bytesRead;

					while ((bytesRead = zipInputStream.read(buffer)) != -1) {
						zipOutputStream.write(buffer, 0, bytesRead);
					}

					zipOutputStream.closeEntry();
				}
			}
		}
	}
}
