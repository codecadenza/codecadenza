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
package net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium;

import java.util.Set;
import net.codecadenza.eclipse.generator.testing.gui.imp.SeleniumGeneratorUtil;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;

/**
 * <p>
 * Generator for test actions that execute a form action
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormActionSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public FormActionSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
		super(testAction, project, declarations);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.testing.gui.action.IGUITestActionGenerator#createTestAction()
	 */
	@Override
	public String createTestAction() {
		final var b = new StringBuilder();
		final Form targetForm = testAction.getTargetForm();

		b.append(openTabPageOfGridPanel());
		b.append(addComment());

		if (testAction.needsTestData())
			b.append(addTestDataInit());

		if (targetForm != null) {
			final var targetFormDeclaration = targetForm.getName() + " " + targetForm.getLowerCaseName();
			final FormTypeEnumeration targetFormType = testAction.getTargetForm().getFormType();

			if (!declarations.contains(targetFormDeclaration)) {
				b.append(targetFormDeclaration);

				declarations.add(targetFormDeclaration);
			}
			else
				b.append(targetForm.getLowerCaseName());

			b.append(" = ");
			b.append(form.getLowerCaseName());

			if (testAction.getFormPanel() != null)
				b.append(".getGridPanel" + testAction.getFormPanel().getAssociation().getUpperCaseName() + "()");

			if (testAction.getFormAction().getType() != ActionType.COPY) {
				if (targetFormType == FormTypeEnumeration.CREATE) {
					if (project.hasJSFClient())
						b.append(".clickMenuBarCreateNew(" + targetForm.getName() + ".class);");
					else
						b.append(".clickButtonCreateNew(" + targetForm.getName() + ".class);");
				}
				else if (targetFormType == FormTypeEnumeration.ADD) {
					if (project.hasJSFClient())
						b.append(".clickMenuBarAddNew(" + targetForm.getName() + ".class);");
					else
						b.append(".clickButtonCreateNew(" + targetForm.getName() + ".class);");
				}
				else if (targetFormType == FormTypeEnumeration.UPDATE)
					b.append(".clickContextMenuUpdate(row, " + targetForm.getName() + ".class);");
				else if (targetFormType == FormTypeEnumeration.READONLY)
					b.append(".clickContextMenuView(row, " + targetForm.getName() + ".class);");
			}
			else if (project.hasAngularClient())
				b.append(".clickContextMenuCopy(row);");
			else
				b.append(".clickContextMenuCopy(row, " + targetForm.getName() + ".class);");
		}
		else {
			b.append(form.getLowerCaseName());

			if (testAction.getFormPanel() != null)
				b.append(".getGridPanel" + testAction.getFormPanel().getAssociation().getUpperCaseName() + "()");

			if (testAction.getFormAction().getType() == ActionType.DELETE)
				b.append(".clickContextMenuDelete(row);");
			else if (testAction.getFormAction().getType() == ActionType.DOWNLOAD_EXPORT) {
				final var exchangeMethod = (DataExchangeMethod) testAction.getFormAction().getBoundaryMethod().getServiceMethod();

				b.append(".clickContextMenuExport(");

				if (exchangeMethod.getSingleObjectFilterParam() != null)
					b.append("row");

				b.append(");");
			}
			else if (testAction.getFormAction().getType() == ActionType.UPLOAD_IMPORT) {
				final var exchangeMethod = (DataExchangeMethod) testAction.getFormAction().getBoundaryMethod().getServiceMethod();

				if (project.hasAngularClient() && !exchangeMethod.getMethodParameters().isEmpty())
					b.append(".pressImportButton(");
				else
					b.append(".clickContextMenuImport(");

				if (!exchangeMethod.getMethodParameters().isEmpty()) {
					b.append("testData.getElementTestDataById(");
					b.append(form.getName() + "." + SeleniumGeneratorUtil.getImportDialogName(testAction.getFormAction()) + ")");
				}

				b.append(");");
			}
			else if (testAction.getFormAction().getType() == ActionType.COPY)
				b.append(".clickContextMenuCopy(row);");
			else if (testAction.getFormAction().getType() == ActionType.DOWNLOAD)
				b.append(".clickContextMenuDownload(row);");
		}

		b.append("\n");
		b.append(addWaitForFeedback());

		return b.toString();
	}

}
