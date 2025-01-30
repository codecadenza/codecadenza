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
package net.codecadenza.eclipse.shared.dialog;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Base class for all internal title area dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class CodeCadenzaTitleAreaDialog extends TitleAreaDialog {
	/**
	 * Create a dialog instance
	 * @param parentShell the parent shell, or <code>null</code> to create a top-level shell
	 */
	protected CodeCadenzaTitleAreaDialog(Shell parentShell) {
		super(parentShell);

		this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL | SWT.RESIZE);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		// Create a dialog area with one grid column and add a separator above the button area
		return createDialogArea(parent, 1, true);
	}

	/**
	 * Create the area of the dialog. The composite that is returned uses a <code>GridLayout</code>.
	 * @param parent the parent composite to contain the dialog area
	 * @param gridColumns the number of columns of the <code>GridLayout</code>
	 * @return the control that represents the dialog area
	 */
	protected Control createDialogArea(Composite parent, int gridColumns) {
		// Create a dialog area with an arbitrary number of grid columns and add a separator above the button area
		return createDialogArea(parent, gridColumns, true);
	}

	/**
	 * Create the area of the dialog. The composite that is returned uses a <code>GridLayout</code>.
	 * @param parent the parent composite to contain the dialog area
	 * @param gridColumns the number of columns of the <code>GridLayout</code>
	 * @param addSeparator controls if a separator above of the button area should be added
	 * @return the control that represents the dialog area
	 */
	protected Control createDialogArea(Composite parent, int gridColumns, boolean addSeparator) {
		// Create the full dialog area with standard margins
		final var panFullArea = (Composite) super.createDialogArea(parent);
		final var glDialogArea = new GridLayout(gridColumns, false);

		// If the grid layout contains more than one column the margins must be adjusted in order to obtain a consistent layout over
		// all dialogs! In this case we assume that the dialog area is directly filled with standard input controls. The standard
		// margins should be used if the layout has only one column. As a rule, the dialog area is filled with one or more panels that
		// in turn have their own margin. As a result, if the width of the dialog area is adjusted, the overall width would become too
		// large.
		if (gridColumns > 1) {
			glDialogArea.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
			glDialogArea.marginWidth = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
		}

		// Create the dialog area that can be used as a container for all controls
		final var panDialogArea = new Composite(panFullArea, SWT.NONE);
		panDialogArea.setLayout(glDialogArea);
		panDialogArea.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (addSeparator) {
			// Add a separator above the button area
			final var separator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
			separator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}

		return panDialogArea;
	}

}
