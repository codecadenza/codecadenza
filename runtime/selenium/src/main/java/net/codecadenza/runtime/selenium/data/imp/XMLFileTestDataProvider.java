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
package net.codecadenza.runtime.selenium.data.imp;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import java.io.File;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import net.codecadenza.runtime.selenium.data.ITestDataProvider;
import net.codecadenza.runtime.selenium.data.PageActionTestData;
import net.codecadenza.runtime.selenium.data.TestCaseData;
import net.codecadenza.runtime.selenium.util.SeleniumTestProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Provider that reads test data for a given test case from a XML file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLFileTestDataProvider implements ITestDataProvider {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String XML_SUFFIX = ".xml";

	private HashMap<String, List<PageActionTestData>> pageMap;
	private HashMap<String, Integer> pageIndexMap;
	private SeleniumTestProperties properties;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.data.ITestDataProvider#
	 * setProperties(net.codecadenza.runtime.selenium.util.SeleniumTestProperties)
	 */
	@Override
	public void setProperties(SeleniumTestProperties properties) {
		this.properties = properties;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.data.ITestDataProvider#init(java.lang.Class)
	 */
	@Override
	public synchronized void init(Class<?> testClass) {
		final var resourceFile = new File(properties.getTestDataPath(), testClass.getSimpleName() + XML_SUFFIX);

		if (!resourceFile.exists()) {
			final var message = "Resource file '" + resourceFile.getName() + "' not found!";

			logger.error(message);

			throw new RuntimeException(message);
		}

		pageIndexMap = new HashMap<>();
		pageMap = new HashMap<>();

		try {
			logger.debug("Read content from file '{}'", resourceFile.getName());

			// Load test data from the respective XML file
			final Unmarshaller unmarshaller = JAXBContext.newInstance(TestCaseData.class).createUnmarshaller();
			final var testCaseData = (TestCaseData) unmarshaller.unmarshal(resourceFile);

			// Iterate over all page action test data objects in order to fill internal data structures
			testCaseData.getPageActionTestData().forEach(pageActionTestData -> {
				if (!pageMap.containsKey(pageActionTestData.getPageClassName())) {
					// Add all page action test data objects of a dedicated page object to a hash map
					pageMap.put(pageActionTestData.getPageClassName(), new ArrayList<>());

					// Initialize the internally used hash map that keeps track of the next page action test data object to be loaded
					pageIndexMap.put(pageActionTestData.getPageClassName(), 0);
				}

				pageMap.get(pageActionTestData.getPageClassName()).add(pageActionTestData);
			});
		}
		catch (final Exception e) {
			final var message = "Error while parsing file '" + resourceFile.getName() + "'!";

			logger.error(message, e);

			throw new RuntimeException(message, e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.selenium.data.ITestDataProvider#getNextTestDataForPage(java.lang.Class)
	 */
	@Override
	public synchronized PageActionTestData getNextTestDataForPage(Class<?> pageClass) {
		if (!pageMap.containsKey(pageClass.getName())) {
			final var message = "Test data for page object of class '" + pageClass.getName() + "' not found!";

			logger.error(message);

			throw new RuntimeException(message);
		}

		// Determine the current list index
		final int index = pageIndexMap.get(pageClass.getName());

		// Null must be returned if no element is available!
		if ((index + 1) > pageMap.get(pageClass.getName()).size())
			return null;

		logger.trace("Read page action test data for page '{}' at position {}", pageClass.getName(), (index + 1));

		// Get the current page action test data for the selected page object
		final PageActionTestData pageActionTestData = pageMap.get(pageClass.getName()).get(index);

		// Increment the index that should be used when invoking this method the next time
		pageIndexMap.put(pageClass.getName(), index + 1);

		return pageActionTestData;
	}

}
