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
package net.codecadenza.eclipse.generator.basic.testing;

import net.codecadenza.eclipse.generator.basic.testing.imp.SeleniumTestingProjectFilesGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;

/**
 * <p>
 * Factory for generators that create basic source and configuration files for testing
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestingProjectFilesGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private TestingProjectFilesGeneratorFactory() {

	}

	/**
	 * Factory method to get a generator implementation depending on the artifact type
	 * @param module
	 * @param artifactType
	 * @return the basic testing project files generator
	 * @throws IllegalStateException if an implementation for the given artifact type is not available
	 */
	public static ITestingProjectFilesGenerator getGenerator(AbstractTestModule module, BuildArtifactType artifactType) {
		if (artifactType == BuildArtifactType.SELENIUM_TEST)
			return new SeleniumTestingProjectFilesGenerator((SeleniumTestModule) module, artifactType);

		throw new IllegalStateException("A generator for the artifact type '" + artifactType + "' is not available!");
	}

}
