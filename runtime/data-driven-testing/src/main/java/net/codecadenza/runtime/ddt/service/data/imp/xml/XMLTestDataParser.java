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
package net.codecadenza.runtime.ddt.service.data.imp.xml;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Unmarshaller;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.model.xml.XMLTestData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Parser that creates a {@link TestData} object from the given XML file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLTestDataParser {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final File testDataFile;

	/**
	 * Constructor
	 * @param testDataFile
	 */
	public XMLTestDataParser(File testDataFile) {
		this.testDataFile = testDataFile;
	}

	/**
	 * Parse the given XML file
	 * @return the {@link TestData} object
	 * @throws IllegalArgumentException if the file doesn't exist or if it is a directory
	 * @throws UncheckedIOException if the content of the file could not be read
	 * @throws IllegalStateException if the XML file could not be parsed
	 */
	public TestData parseTestData() {
		if (testDataFile == null)
			throw new IllegalArgumentException("The field 'testDataFile' must not be null!");
		else if (!testDataFile.exists())
			throw new IllegalArgumentException("The file '" + testDataFile.getName() + "' doesn't exist!");
		else if (testDataFile.isDirectory())
			throw new IllegalArgumentException("The file '" + testDataFile.getName() + "' is a directory!");

		logger.debug("Parsing test data from file '{}'", testDataFile.getName());

		try (final InputStream inputStream = new FileInputStream(testDataFile)) {
			final JAXBContext context = JAXBContext.newInstance(XMLTestData.class);
			final Unmarshaller unmarshaller = context.createUnmarshaller();
			return (TestData) unmarshaller.unmarshal(inputStream);
		}
		catch (final IOException ex) {
			throw new UncheckedIOException("Error loading test data from file " + testDataFile.getName(), ex);
		}
		catch (final JAXBException ex) {
			throw new IllegalStateException("Error parsing test data form file " + testDataFile.getName());
		}
	}

}
