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

import static net.codecadenza.eclipse.shared.Constants.INDEX_PAGE_NAME;

import java.util.Set;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;

/**
 * <p>
 * Generator for test actions that open a page by using the main navigator
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PageNavigatorSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public PageNavigatorSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
		super(testAction, project, declarations);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.testing.gui.action.IGUITestActionGenerator#createTestAction()
	 */
	@Override
	public String createTestAction() {
		final var b = new StringBuilder();
		final boolean openIndexPage = testAction.getTestCase().getTestActions().indexOf(testAction) == 0;

		b.append(addComment());
		b.append(addTestDataInit());

		// If this action is the initial action of the test case the index page must be opened first in order to use the navigator
		// component
		if (openIndexPage) {
			b.append("\n");
			b.append("// Open index page\n");
			b.append("var indexPage = new " + INDEX_PAGE_NAME + "(testContext);\n");
			b.append("indexPage.open(\"\");\n\n");
		}

		b.append(addPageObjectDeclaration());
		b.append(" = ");

		if (!openIndexPage) {
			b.append("new " + form.getName() + "(testContext);\n");
			b.append(form.getLowerCaseName());
		}
		else
			b.append("indexPage");

		b.append(".openPageByNavigator(testData.getNavigationTarget(), " + form.getName() + ".class);\n");
		b.append(addPageTitleValidation(form));
		b.append(addWaitForFeedback());

		return b.toString();
	}

}
