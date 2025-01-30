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
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;

/**
 * <p>
 * Generator for test actions that search and select a row in either a view form or a grid panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RowSelectionSeleniumTestActionGenerator extends AbstractSeleniumTestActionGenerator {
	/**
	 * Constructor
	 * @param testAction
	 * @param project
	 * @param declarations
	 */
	public RowSelectionSeleniumTestActionGenerator(GUITestAction testAction, Project project, Set<String> declarations) {
		super(testAction, project, declarations);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.testing.gui.action.IGUITestActionGenerator#createTestAction()
	 */
	@Override
	public String createTestAction() {
		final var b = new StringBuilder();
		final var rowDeclaration = "WebElement row";

		b.append(openTabPageOfGridPanel());
		b.append(addComment());
		b.append(addTestDataInit());

		if (!declarations.contains(rowDeclaration)) {
			b.append(rowDeclaration);

			declarations.add(rowDeclaration);
		}
		else
			b.append("row");

		b.append(" = " + form.getLowerCaseName());

		if (testAction.getFormPanel() != null)
			b.append(".getGridPanel" + testAction.getFormPanel().getAssociation().getUpperCaseName() + "()");

		if (testAction.getTestData().isEmpty())
			throw new IllegalStateException(
					"The test action for selecting a row could not be created as no respective test data object has been found!");

		final GUITestData testData = testAction.getTestData().get(0);

		if (testData.getType() == GUITestDataType.OBJECT_ID)
			b.append(".getRowByObjectId(testData.getObjectId()");
		else if (testData.getType() == GUITestDataType.CELL_VALUE)
			b.append(".getRowByCellValue(testData.getElementTestDataById(PageElementTestData.CELL_VALUE_ID).getFilterValue()");
		else if (testData.getType() == GUITestDataType.ROW_INDEX) {
			b.append(".getRowByRowIndex(Integer.parseInt(");
			b.append("testData.getElementTestDataById(PageElementTestData.ROW_INDEX_ID).getFilterValue())");
		}

		if (testData.getType() != GUITestDataType.ROW_INDEX && testAction.getType() == GUITestActionType.SEARCH_ROW_CURRENT_PAGE)
			b.append(", true");

		b.append(");\n");

		return b.toString();
	}

}
