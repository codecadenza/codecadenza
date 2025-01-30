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
package net.codecadenza.eclipse.tools.sqleditor.actions;

import net.codecadenza.eclipse.tools.sqleditor.SQLEditor;
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
public class SQLEditorToolBar {
	private final SQLLimitRowsControl limitRows;
	private final CommitModeControl commitMode;
	private final ExecuteSQLAction executeAction;
	private final CommitAction commitAction;
	private final RollbackAction rollbackAction;

	/**
	 * Constructor
	 * @param parent
	 * @param editor
	 */
	public SQLEditorToolBar(Composite parent, SQLEditor editor) {
		final var coolBar = new CoolBar(parent, SWT.FLAT);
		coolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Initialize the default actions
		executeAction = new ExecuteSQLAction(editor);

		commitAction = new CommitAction(editor);
		commitAction.setEnabled(false);

		rollbackAction = new RollbackAction(editor);
		rollbackAction.setEnabled(false);

		final var toolBarManagerDefault = new ToolBarManager(SWT.FLAT);
		toolBarManagerDefault.add(executeAction);
		toolBarManagerDefault.add(new ClearTextAction(editor));
		toolBarManagerDefault.add(commitAction);
		toolBarManagerDefault.add(rollbackAction);
		toolBarManagerDefault.add(new SaveFileContentAction(editor));

		// Add a control to limit the number of returned records
		limitRows = new SQLLimitRowsControl();

		// Add a control to set the transaction commit mode
		commitMode = new CommitModeControl();

		final var toolBarManagerAdd = new ToolBarManager(SWT.FLAT);
		toolBarManagerAdd.add(limitRows);
		toolBarManagerAdd.add(commitMode);

		final var coolBarManager = new CoolBarManager(coolBar);
		coolBarManager.add(new ToolBarContributionItem(toolBarManagerDefault));
		coolBarManager.add(new ToolBarContributionItem(new ToolBarManager(SWT.FLAT)));
		coolBarManager.add(new ToolBarContributionItem(toolBarManagerAdd));
		coolBarManager.update(true);
	}

	/**
	 * Enable the manual commit mode
	 */
	public void finishTransaction() {
		rollbackAction.setEnabled(false);
		commitAction.setEnabled(false);
		executeAction.setEnabled(true);
	}

	/**
	 * Disable all actions
	 */
	public void disableActions() {
		executeAction.setEnabled(false);
		rollbackAction.setEnabled(false);
		commitAction.setEnabled(false);
	}

	/**
	 * Enable all actions
	 */
	public void enableActions() {
		executeAction.setEnabled(true);

		if (commitMode.isAutoCommit()) {
			rollbackAction.setEnabled(false);
			commitAction.setEnabled(false);
		}
		else {
			rollbackAction.setEnabled(true);
			commitAction.setEnabled(true);
		}
	}

	/**
	 * Get the maximum number of records to be returned by a query
	 * @return the maximum number of rows to fetch. The method will return 0 if all available rows should be fetched!
	 */
	public int getLimitResults() {
		return limitRows.getLimitResults();
	}

	/**
	 * Get the transaction mode
	 * @return true if the auto-commit mode is active
	 */
	public boolean isAutoCommit() {
		return commitMode.isAutoCommit();
	}

}
