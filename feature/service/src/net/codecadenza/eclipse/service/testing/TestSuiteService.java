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
package net.codecadenza.eclipse.service.testing;

import net.codecadenza.eclipse.generator.testing.gui.GUITestGenerator;
import net.codecadenza.eclipse.generator.testing.gui.GUITestGeneratorFactory;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service for test suites
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestSuiteService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public TestSuiteService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild the test suite source file
	 * @param testSuite
	 * @throws Exception if the rebuild operation has failed due to an internal error
	 */
	public void rebuildTestSuiteSourceFile(TestSuite testSuite) throws Exception {
		final GUITestGenerator generator = GUITestGeneratorFactory.getGenerator(testSuite.getTestModule());
		generator.createTestSuite(testSuite);
	}

	/**
	 * Delete the given test suite and remove the respective source file
	 * @param testSuite
	 * @throws Exception if the delete operation has failed due to an internal error
	 */
	public void deleteTestSuite(TestSuite testSuite) throws Exception {
		// Remove the test suite from the meta-model
		project.eResource().getContents().remove(testSuite);

		EclipseIDEService.saveProjectMetaData(project);

		EclipseIDEService.deleteSource(testSuite.getSourceFile());
	}

}
