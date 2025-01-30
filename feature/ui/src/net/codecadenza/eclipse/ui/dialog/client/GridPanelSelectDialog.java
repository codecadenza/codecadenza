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
package net.codecadenza.eclipse.ui.dialog.client;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for selecting an existing grid panel for a given form
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GridPanelSelectDialog extends CodeCadenzaDialog {
	private DataComboViewer<FormPanel> cboGridPanel;
	private FormPanel selectedPanel;
	private final Form form;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param form
	 */
	public GridPanelSelectDialog(Shell parentShell, Form form) {
		super(parentShell);

		this.form = form;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);
		final var panels = new BasicEList<FormPanel>();

		final var lblBasePanel = new Label(panDialogArea, SWT.NONE);
		lblBasePanel.setText("Base panel:");

		final var gdGridPanel = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdGridPanel.widthHint = 300;

		cboGridPanel = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(FormPanel element) {
				return element.getName();
			}
		};

		cboGridPanel.setLayoutData(gdGridPanel);

		final Project project = form.getDTO().getNamespace().getProject();

		for (final FormPanel p : project.getAllGridPanelsOfProject()) {
			if (p.getBasePanel() != null || p.getAssociation() == null)
				continue;

			// Check if this grid panel can be used for the selected form!
			if (!form.getDTO().getDomainObject().getAllAssociations().contains(p.getAssociation()))
				continue;

			boolean found = false;

			for (final FormPanel existingPanel : form.getFormPanels()) {
				if (existingPanel.getBasePanel() == null)
					continue;

				final FormPanel basePanel = existingPanel.getBasePanel();

				// Check if the form already uses the "same" grid panel!
				if (basePanel.equals(p)) {
					found = true;
					break;
				}
			}

			// An existing grid panel shouldn't be added to the list!
			if (!found)
				panels.add(p);
		}

		// Add all available grid panels
		cboGridPanel.setData(panels);

		if (!panels.isEmpty())
			cboGridPanel.setSelectedItem(panels.get(0));

		return panDialogArea;
	}

	/**
	 * @return the selected form panel
	 */
	public FormPanel getSelectedFormPanel() {
		return selectedPanel;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID)
			selectedPanel = cboGridPanel.getSelectedItem();

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Select new grid panel");
	}

}
