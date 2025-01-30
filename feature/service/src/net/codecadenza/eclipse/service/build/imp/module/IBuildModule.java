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
package net.codecadenza.eclipse.service.build.imp.module;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.jdt.core.IClasspathEntry;

/**
 * <p>
 * Interface that must be implemented by all build modules
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IBuildModule {
	/**
	 * @return a list of project natures
	 */
	List<String> getNatures();

	/**
	 * @return a list containing all folders
	 */
	List<String> getFolders();

	/**
	 * @param projectName
	 * @return a hash set containing necessary classpath entries
	 */
	HashSet<IClasspathEntry> getClassPathEntries(String projectName);

	/**
	 * @return a list containing required packages
	 */
	List<String> getPackages();

	/**
	 * @return a list containing required packages for tests
	 */
	List<String> getTestPackages();

	/**
	 * @return a map with compiler options
	 */
	Map<String, String> getCompilerOptions();

	/**
	 * @return a list with provided configuration files
	 */
	List<WorkspaceFile> getConfigurationFiles();

	/**
	 * Create the initial source files that this build module must provide
	 * @throws Exception if an internal error has occurred
	 */
	void createInitialSourceFiles() throws Exception;

}
