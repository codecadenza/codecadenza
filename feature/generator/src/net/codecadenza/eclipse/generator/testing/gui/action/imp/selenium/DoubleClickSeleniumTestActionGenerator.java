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
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;

/**
 * <p>
 * Generator for test actions that open a target form by performing a double-click on a table row
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DoubleClickSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public DoubleClickSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
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
		final var targetFormDeclaration = targetForm.getName() + " " + targetForm.getLowerCaseName();

		b.append(openTabPageOfGridPanel());
		b.append(addComment());

		if (testAction.needsTestData())
			b.append(addTestDataInit());

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

		b.append(".doubleClickRow(row, " + targetForm.getName() + ".class);\n");
		b.append(addWaitForFeedback());

		return b.toString();
	}

}
