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
package net.codecadenza.eclipse.tools.jpaeditor.actions;

import net.codecadenza.eclipse.tools.jpaeditor.JPAQueryEditor;
import org.eclipse.jface.action.CoolBarManager;
import org.eclipse.jface.action.ToolBarContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.CoolBar;

/**
 * <p>
 * Editor toolbar
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAEditorToolBar {
	private final JPALimitRowsControl limitRows;

	/**
	 * Constructor
	 * @param parent
	 * @param editor
	 */
	public JPAEditorToolBar(Composite parent, JPAQueryEditor editor) {
		final var coolBar = new CoolBar(parent, SWT.FLAT);
		coolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Initialize the actions and add them to the toolbar
		final var toolBarManagerDefault = new ToolBarManager(SWT.FLAT);
		toolBarManagerDefault.add(new ExecuteJPAAction(editor));
		toolBarManagerDefault.add(new ClearTextAction(editor));
		toolBarManagerDefault.add(new SaveFileContentAction(editor));

		// Add a control to limit the number of objects a query should return
		limitRows = new JPALimitRowsControl();

		final var toolBarManagerLimit = new ToolBarManager(SWT.FLAT);
		toolBarManagerLimit.add(limitRows);

		final var coolBarManager = new CoolBarManager(coolBar);
		coolBarManager.add(new ToolBarContributionItem(toolBarManagerDefault));
		coolBarManager.add(new ToolBarContributionItem(new ToolBarManager(SWT.FLAT)));
		coolBarManager.add(new ToolBarContributionItem(toolBarManagerLimit));
		coolBarManager.update(true);
	}

	/**
	 * Get the maximum number of entities to be returned by a query
	 * @return the maximum number of rows to fetch. The method will return 0 if all available rows should be fetched!
	 */
	public int getLimitResults() {
		return limitRows.getLimitResults();
	}

}
