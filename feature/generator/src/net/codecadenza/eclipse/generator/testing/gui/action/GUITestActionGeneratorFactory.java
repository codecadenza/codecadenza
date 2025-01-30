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
package net.codecadenza.eclipse.generator.testing.gui.action;

import java.util.Set;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.CountSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.DoubleClickSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.EnterDataSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.FileUploadSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.FormActionSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.LoginSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.LogoutSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.PageDirectSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.PageNavigatorSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.PressButtonSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.RefreshSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.ResetSearchSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.RowCountSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.RowSelectionSeleniumTestActionGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.imp.selenium.SearchInputSeleniumTestActionGenerator;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;

/**
 * <p>
 * Factory for GUI test action generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestActionGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private GUITestActionGeneratorFactory() {

	}

	/**
	 * @param testAction
	 * @param project
	 * @param declarations
	 * @return the GUI test action generator based on the given type
	 * @throws IllegalStateException if a generator for the given GUI test action type is not available
	 */
	public static IGUITestActionGenerator getGUITestActionGenerator(GUITestAction testAction, Project project,
			Set<String> declarations) {
		final BuildArtifactType artifactType = testAction.getTestCase().getTestModule().getArtifactType();
		final GUITestActionType type = testAction.getType();

		// Currently, only Selenium is supported!
		if (artifactType != BuildArtifactType.SELENIUM_TEST)
			throw new IllegalStateException(
					"A GUI test action generator for the artifact type '" + artifactType + "' is not available!");

		if (type == GUITestActionType.OPEN_PAGE_DIRECT)
			return new PageDirectSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.OPEN_PAGE_BY_NAVIGATOR)
			return new PageNavigatorSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.ENTER_FORM_DATA || type == GUITestActionType.VALIDATE_FORM_DATA)
			return new EnterDataSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.PRESS_CANCEL_BUTTON || type == GUITestActionType.PRESS_OK_BUTTON
				|| type == GUITestActionType.PRESS_DOWNLOAD_BUTTON)
			return new PressButtonSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.EXECUTE_REFRESH_ACTION)
			return new RefreshSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.SEARCH_ROW_ALL_PAGES || type == GUITestActionType.SEARCH_ROW_CURRENT_PAGE)
			return new RowSelectionSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.EXECUTE_FORM_ACTION)
			return new FormActionSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.UPLOAD_FILE)
			return new FileUploadSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.DOUBLE_CLICK_ROW)
			return new DoubleClickSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.ENTER_SEARCH_DATA)
			return new SearchInputSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.COUNT_RECORDS)
			return new CountSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.RESET_SEARCH_DATA)
			return new ResetSearchSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.VALIDATE_ROW_COUNT_EQUAL || type == GUITestActionType.VALIDATE_ROW_COUNT_GREATER
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_SMALLER)
			return new RowCountSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.PERFORM_LOGOUT)
			return new LogoutSeleniumTestActionGenerator(testAction, project, declarations);
		else if (type == GUITestActionType.OPEN_LOGIN_PAGE)
			return new LoginSeleniumTestActionGenerator(testAction, project, declarations);

		throw new IllegalStateException("A GUI test action generator for the action type '" + type + "' is not available!");
	}

}
