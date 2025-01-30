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
package net.codecadenza.eclipse.generator.common;

import java.util.Set;

/**
 * <p>
 * Interface that must be implemented by all method generator utility classes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IMethodGeneratorUtility {
	/**
	 * @return the default name of the repository
	 */
	String getRepositoryName();

	/**
	 * @return all necessary imports
	 */
	Set<String> getImports();

	/**
	 * @param addQualifier
	 * @param useParameterNames if false we use generated default parameter names (e.g. param1). This is necessary for signature
	 *          compare operations.
	 * @return the method signature
	 */
	String getMethodSignature(boolean addQualifier, boolean useParameterNames);

	/**
	 * @return all necessary imports for the interface
	 */
	Set<String> getInterfaceImports();

	/**
	 * @return all declarations of local services this method needs
	 */
	String getLocalServices();

}
