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
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;

/**
 * <p>
 * Generator for test actions that press a form button
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PressButtonSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public PressButtonSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
		super(testAction, project, declarations);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.testing.gui.action.IGUITestActionGenerator#createTestAction()
	 */
	@Override
	public String createTestAction() {
		final var b = new StringBuilder();
		b.append(addComment());

		if (testAction.needsTestData())
			b.append(addTestDataInit());

		if (testAction.getType() == GUITestActionType.PRESS_CANCEL_BUTTON) {
			if (project.hasVaadinClient())
				b.append(form.getLowerCaseName() + ".pressCancelButton();\n");
			else
				b.append(form.getLowerCaseName() + ".pressBackButton();\n");
		}
		else if (testAction.getType() == GUITestActionType.PRESS_OK_BUTTON) {
			final Form targetForm = testAction.getTargetForm();

			if (targetForm != null) {
				final var targetPageObjectDeclaration = targetForm.getName() + " " + targetForm.getLowerCaseName();

				if (!declarations.contains(targetPageObjectDeclaration)) {
					b.append(targetPageObjectDeclaration);

					declarations.add(targetPageObjectDeclaration);
				}
				else
					b.append(targetForm.getLowerCaseName());

				b.append(" = ");
			}

			b.append(form.getLowerCaseName() + ".pressSaveButton(");

			if (targetForm != null)
				b.append(targetForm.getName() + ".class");

			b.append(");\n");
		}
		else if (testAction.getType() == GUITestActionType.PRESS_DOWNLOAD_BUTTON) {
			final String buttonName = SeleniumGeneratorUtil.getDownloadButtonName(testAction.getFormAction());

			b.append(form.getLowerCaseName() + ".pressButton(" + form.getName() + "." + buttonName + ");\n");
		}

		if (testAction.getActionResult() != null) {
			b.append(addWaitForFeedback());
			b.append("\n" + form.getLowerCaseName() + ".");

			// Finally, the dialog must be closed if further test actions should be processed!
			if (project.hasVaadinClient())
				b.append("pressCancelButton();\n");
			else
				b.append("pressBackButton();\n");
		}

		return b.toString();
	}

}
