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
package net.codecadenza.eclipse.generator.client.imp.eclipse.file;

/**
 * <p>
 * Interface for file handling generators of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface IEclipseFileHandlingGenerator {
	/**
	 * Add all necessary imports
	 */
	void addImports();

	/**
	 * Create the fragment for performing a file download
	 * @param invocationParameter
	 * @return the generated content
	 */
	String createDownloadFragment(String invocationParameter);

	/**
	 * Create the fragment to start an export operation
	 * @return the generated content
	 */
	String createExportInvocationFragment();

	/**
	 * Create the fragment to download generated content of a data export operation
	 * @return the generated content
	 */
	String createDownloadFragmentForExport();

	/**
	 * Create the fragment to upload data that should be processed within a data import operation
	 * @param addFragmentToForm
	 * @return the generated content
	 */
	String createUploadFragmentForImport(boolean addFragmentToForm);

}
