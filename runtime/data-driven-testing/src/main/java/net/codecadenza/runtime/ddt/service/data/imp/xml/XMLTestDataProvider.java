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

import java.io.File;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;
import net.codecadenza.runtime.ddt.service.data.TestDataProviderProperties;
import net.codecadenza.runtime.ddt.service.data.imp.AbstractTestDataProvider;

/**
 * <p>
 * A {@link ITestDataProvider} that loads {@link TestData} from an XML file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLTestDataProvider extends AbstractTestDataProvider<File> {
	/**
	 * Constructor
	 * @param testDataFile the XML file that contains the test data
	 * @param properties the test properties
	 */
	public XMLTestDataProvider(File testDataFile, TestDataProviderProperties properties) {
		super(testDataFile, properties);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.service.ITestDataProvider#loadTestData()
	 */
	@Override
	public TestData loadTestData() {
		final var parser = new XMLTestDataParser(identifier);
		testData = parser.parseTestData();

		init(testData);

		return testData;
	}

}
