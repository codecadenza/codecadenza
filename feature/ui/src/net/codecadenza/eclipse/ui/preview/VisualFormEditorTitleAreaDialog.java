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
package net.codecadenza.eclipse.ui.preview;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeController;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Visual editor that displays a preview of a selected form in a title area dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VisualFormEditorTitleAreaDialog extends TitleAreaDialog implements PreviewChangeListener {
	private final PreviewChangeController eventController = new PreviewChangeController(this);
	private Form form;
	private FormTypeEnumeration formType;
	private Composite panDialogArea;
	private VisualFormEditorPreviewBuilder previewBuilder;

	/**
	 * Constructor
	 * @param parentShell
	 * @param form
	 */
	public VisualFormEditorTitleAreaDialog(Shell parentShell, Form form) {
		this(parentShell);

		this.form = form;
		this.formType = form.getFormType();
	}

	/**
	 * Constructor
	 * @param parentShell
	 */
	private VisualFormEditorTitleAreaDialog(Shell parentShell) {
		super(parentShell);

		this.setShellStyle(super.getShellStyle() | SWT.RESIZE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener#onPreviewChanged()
	 */
	@Override
	public void onPreviewChanged() {
		for (final Control control : panDialogArea.getChildren())
			control.dispose();

		generatePreview();

		panDialogArea.layout(true);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		panDialogArea = (Composite) super.createDialogArea(parent);
		previewBuilder = new VisualFormEditorPreviewBuilder(panDialogArea, eventController);

		generatePreview();

		final var separator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
		separator.setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM, true, false));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(form.getTitle());

		// Don't track resize events for views and list-of-values!
		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW)
			return;

		if (formType == FormTypeEnumeration.LOV)
			return;

		newShell.addListener(SWT.Resize, _ -> {
			form.setHeight(newShell.getSize().y);
			form.setWidth(newShell.getSize().x);
		});
	}

	/**
	 * Generate the preview
	 */
	private void generatePreview() {
		setMessage("Preview initialized successfully!");
		setTitle("Preview mode for " + form.getName());

		try {
			previewBuilder.generateFormPreview(form);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
			setErrorMessage("The preview could not be created! Message: " + e.getMessage());
		}
	}

}
