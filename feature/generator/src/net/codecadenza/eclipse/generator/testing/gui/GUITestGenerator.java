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
package net.codecadenza.eclipse.generator.testing.gui;

import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.TestSuite;

/**
 * <p>
 * Interface that must be implemented by all GUI test generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface GUITestGenerator {
	/**
	 * Create the GUI test case
	 * @param testCase
	 * @throws Exception if the generation of the test case source file has failed
	 */
	void createTestCase(GUITestCase testCase) throws Exception;

	/**
	 * Create the test data file for a given test case
	 * @param testCase
	 * @throws Exception if the generation of the test data file has failed
	 */
	void createTestData(GUITestCase testCase) throws Exception;

	/**
	 * Create the test suite
	 * @param testSuite
	 * @throws Exception if the generation of the test suite source file has failed
	 */
	void createTestSuite(TestSuite testSuite) throws Exception;

}
