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

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.rap.RAPFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.rcp.RCPFileHandlingGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for handling files in an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseFileHandlingGenerator {
	private final AbstractJavaSourceGenerator formGenerator;
	private final BoundaryMethod method;
	private final RichClientI18NGenerator i18n;
	private final Project project;
	private FormAction action;

	/**
	 * Constructor
	 * @param formGenerator
	 * @param method
	 * @param i18n
	 */
	public EclipseFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, BoundaryMethod method,
			RichClientI18NGenerator i18n) {
		this.formGenerator = formGenerator;
		this.method = method;
		this.i18n = i18n;
		this.project = method.getBoundaryBean().getNamespace().getProject();
	}

	/**
	 * Constructor
	 * @param formGenerator
	 * @param action
	 * @param i18n
	 */
	public EclipseFileHandlingGenerator(AbstractJavaSourceGenerator formGenerator, FormAction action,
			RichClientI18NGenerator i18n) {
		this(formGenerator, action.getBoundaryMethod(), i18n);

		this.action = action;
	}

	/**
	 * Add all necessary imports for the given action
	 */
	public void addImports() {
		if (project.hasRCPClient()) {
			if (action != null)
				new RCPFileHandlingGenerator(formGenerator, action, i18n).addImports();
			else
				new RCPFileHandlingGenerator(formGenerator, method, i18n).addImports();

			return;
		}

		if (action != null)
			new RAPFileHandlingGenerator(formGenerator, action, i18n).addImports();
		else
			new RAPFileHandlingGenerator(formGenerator, method, i18n).addImports();
	}

	/**
	 * @param invocationParameter
	 * @return the generated content
	 */
	public String createDownloadFragment(String invocationParameter) {
		if (project.hasRCPClient())
			return new RCPFileHandlingGenerator(formGenerator, method, i18n).createDownloadFragment(invocationParameter);

		return new RAPFileHandlingGenerator(formGenerator, method, i18n).createDownloadFragment(invocationParameter);
	}

	/**
	 * @return the generated content
	 */
	public String createExportInvocationFragment() {
		if (project.hasRCPClient())
			return new RCPFileHandlingGenerator(formGenerator, method, i18n).createExportInvocationFragment();

		return new RAPFileHandlingGenerator(formGenerator, method, i18n).createExportInvocationFragment();
	}

	/**
	 * @return the generated content
	 */
	public String createDownloadFragmentForExport() {
		if (project.hasRCPClient())
			return new RCPFileHandlingGenerator(formGenerator, method, i18n).createDownloadFragmentForExport();

		return new RAPFileHandlingGenerator(formGenerator, method, i18n).createDownloadFragmentForExport();
	}

	/**
	 * @param addFragmentToForm
	 * @return the generated content
	 */
	public String createUploadFragmentForImport(boolean addFragmentToForm) {
		if (project.hasRCPClient())
			return new RCPFileHandlingGenerator(formGenerator, method, i18n).createUploadFragmentForImport(addFragmentToForm);

		return new RAPFileHandlingGenerator(formGenerator, method, i18n).createUploadFragmentForImport(addFragmentToForm);
	}

}
