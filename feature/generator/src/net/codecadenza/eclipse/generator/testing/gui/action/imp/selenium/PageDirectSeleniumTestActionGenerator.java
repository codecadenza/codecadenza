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
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;

/**
 * <p>
 * Generator for test actions that open a page directly
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PageDirectSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public PageDirectSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
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

		b.append(addPageObjectDeclaration());
		b.append(" = new " + form.getName() + "(testContext);\n");
		b.append(form.getLowerCaseName() + ".open(" + form.getName() + ".RESOURCE_PATH");

		for (final GUITestData testData : testAction.getTestData())
			if (testData.getType() == GUITestDataType.OBJECT_ID) {
				b.append(", testData.getObjectId()");
				break;
			}

		b.append(");\n");
		b.append(addPageTitleValidation(form));
		b.append(addWaitForFeedback());

		return b.toString();
	}

}
