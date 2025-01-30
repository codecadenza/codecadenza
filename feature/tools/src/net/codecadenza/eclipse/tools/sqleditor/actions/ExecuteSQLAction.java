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

import static net.codecadenza.eclipse.shared.Constants.IMG_EXEC_SQL;

import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.sqleditor.SQLEditor;
import org.eclipse.jface.resource.ImageDescriptor;

/**
 * <p>
 * Action to execute SQL statements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExecuteSQLAction extends AbstractEditorAction {
	/**
	 * Constructor
	 * @param editor
	 */
	public ExecuteSQLAction(SQLEditor editor) {
		super(editor);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#getImageDescriptor()
	 */
	@Override
	public ImageDescriptor getImageDescriptor() {
		return CodeCadenzaResourcePlugin.getImageDescriptor(IMG_EXEC_SQL);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#getText()
	 */
	@Override
	public String getText() {
		return "Execute SQL";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.sqleditor.actions.AbstractEditorAction#getToolTipText()
	 */
	@Override
	public String getToolTipText() {
		return "Execute SQL";
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.Action#run()
	 */
	@Override
	public void run() {
		editor.processAllSQLStatements();
	}

}
