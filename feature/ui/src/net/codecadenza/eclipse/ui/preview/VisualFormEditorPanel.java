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

import static net.codecadenza.eclipse.shared.Constants.IMG_TILE_BAR_BUTTONS;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.form.init.UpdateFormInitService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeController;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

/**
 * <p>
 * Panel that displays a preview of a selected form or grid panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VisualFormEditorPanel extends Composite implements PreviewChangeListener {
	private static final int DEFAULT_WIDTH = 600;
	private static final int DEFAULT_HEIGHT = 500;
	private static final int TITLE_AREA_HIGHT = 70;

	private final PreviewChangeController eventController = new PreviewChangeController(this);
	private final Color backgroundColor = Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);
	private final Color windowTitleBarColor = new Color(Display.getCurrent(), 0xC9, 0xC9, 0xC9);
	private final String title;
	private Form form;
	private FormPanel gridPanel;
	private VisualFormEditorPreviewBuilder previewBuilder;
	private Composite panPreview;
	private int width = DEFAULT_WIDTH;
	private int height = DEFAULT_HEIGHT;
	private boolean editMode = true;
	private boolean grabExessSpace;
	private boolean addTitleArea;
	private boolean addButtonBar;
	private UpdateFormInitService initService;
	private GridData gdForm;
	private Label lblWindowTitle;
	private final String dialogTitle;

	/**
	 * Constructor
	 * @param panParent
	 * @param form
	 * @param editMode
	 * @param initService
	 */
	public VisualFormEditorPanel(Composite panParent, Form form, boolean editMode, UpdateFormInitService initService) {
		super(panParent, SWT.NONE);

		this.form = form;
		this.editMode = editMode;
		this.initService = initService;
		this.title = form.getTitle();
		this.dialogTitle = "Change layout of form";

		if (form.getWidth() != 0)
			this.width = form.getWidth();

		if (form.getHeight() != 0)
			this.height = form.getHeight();

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW || form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW
				|| form.getFormType() == FormTypeEnumeration.LOV)
			this.grabExessSpace = true;
		else {
			this.addButtonBar = true;
			this.grabExessSpace = !initService.isOpenedInNewWindow();
			this.addTitleArea = !this.grabExessSpace && form.isTitleArea();
		}

		initialize();
	}

	/**
	 * Constructor
	 * @param panParent
	 * @param gridPanel
	 * @param editMode
	 */
	public VisualFormEditorPanel(Composite panParent, FormPanel gridPanel, boolean editMode) {
		super(panParent, SWT.NONE);

		this.gridPanel = gridPanel;
		this.editMode = editMode;
		this.grabExessSpace = true;
		this.dialogTitle = "Change layout of grid panel";
		this.title = gridPanel.getLabel();

		initialize();
	}

	/**
	 * @param listener
	 */
	public void addListener(PreviewChangeListener listener) {
		eventController.addListener(listener);
	}

	/**
	 * @param width
	 */
	public void setFormWidth(int width) {
		if (form == null)
			return;

		form.setWidth(width);
		gdForm.widthHint = width;

		layout(true);
	}

	/**
	 * @param height
	 */
	public void setFormHeight(int height) {
		if (form == null)
			return;

		form.setHeight(height);
		gdForm.heightHint = height;

		layout(true);
	}

	/**
	 * @param title
	 */
	public void setFormTitle(String title) {
		if (form != null)
			form.setTitle(title);

		lblWindowTitle.setText(title);
	}

	/**
	 * @param addTitleArea
	 */
	public void setTitleArea(boolean addTitleArea) {
		if (form == null)
			return;

		form.setTitleArea(addTitleArea);
		this.addTitleArea = addTitleArea;

		// All existing components must be disposed!
		for (final Control control : this.getChildren())
			control.dispose();

		// Reinitialize the component and generate the preview
		initialize();
		generatePreview();

		layout(true);
	}

	/**
	 * Initialize the panel
	 */
	private void initialize() {
		final var glThis = new GridLayout();
		glThis.verticalSpacing = 0;
		glThis.horizontalSpacing = 0;
		glThis.marginHeight = 20;
		glThis.marginWidth = 20;

		setBackground(backgroundColor);
		setLayout(glThis);

		final var glForm = new GridLayout();
		glForm.verticalSpacing = 0;
		glForm.horizontalSpacing = 0;
		glForm.marginHeight = 0;
		glForm.marginWidth = 0;

		gdForm = new GridData(SWT.LEFT, SWT.TOP, false, false);

		if (grabExessSpace)
			gdForm = new GridData(SWT.FILL, SWT.FILL, true, true);

		gdForm.heightHint = height;
		gdForm.widthHint = width;

		final var panForm = new Composite(this, SWT.BORDER);
		panForm.setLayout(glForm);
		panForm.setLayoutData(gdForm);

		// Add the window bar
		final var glWindowBorder = new GridLayout(2, false);
		glWindowBorder.marginHeight = 0;
		glWindowBorder.horizontalSpacing = 0;
		glWindowBorder.marginWidth = 0;
		glWindowBorder.verticalSpacing = 0;

		final var gdWindowBorder = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdWindowBorder.heightHint = 25;

		final var panWindowBorder = new Composite(panForm, SWT.NONE);
		panWindowBorder.setLayout(glWindowBorder);
		panWindowBorder.setLayoutData(gdWindowBorder);
		panWindowBorder.setBackground(windowTitleBarColor);

		final var gdWindowTitle = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdWindowTitle.horizontalIndent = 10;

		lblWindowTitle = new Label(panWindowBorder, SWT.NONE);
		lblWindowTitle.setText(title);
		lblWindowTitle.setFont(JFaceResources.getBannerFont());
		lblWindowTitle.setLayoutData(gdWindowTitle);
		lblWindowTitle.setBackground(panWindowBorder.getBackground());

		final var lblWindowButtons = new Label(panWindowBorder, SWT.NONE);
		lblWindowButtons.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TILE_BAR_BUTTONS));
		lblWindowButtons.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));

		final var lblWindowSeparator = new Label(panWindowBorder, SWT.SEPARATOR | SWT.HORIZONTAL);
		lblWindowSeparator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

		if (addTitleArea) {
			// Add the title area
			final var gdTitleArea = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdTitleArea.heightHint = TITLE_AREA_HIGHT;

			final var glTitleArea = new GridLayout(2, false);
			glTitleArea.verticalSpacing = 0;
			glTitleArea.horizontalSpacing = 0;
			glTitleArea.marginBottom = 0;
			glTitleArea.marginHeight = 0;
			glTitleArea.marginWidth = 0;
			glTitleArea.marginRight = 0;
			glTitleArea.marginLeft = 0;
			glTitleArea.marginTop = 0;

			final var panTitleArea = new Composite(panForm, SWT.NONE);
			panTitleArea.setBackground(backgroundColor);
			panTitleArea.setLayout(glTitleArea);
			panTitleArea.setLayoutData(gdTitleArea);

			final var gdTitle = new GridData(SWT.LEFT, SWT.CENTER, true, false);
			gdTitle.verticalIndent = 10;
			gdTitle.horizontalIndent = 20;

			final var lblTitle = new Label(panTitleArea, SWT.NONE);
			lblTitle.setText(dialogTitle);
			lblTitle.setLayoutData(gdTitle);
			lblTitle.setFont(JFaceResources.getBannerFont());
			lblTitle.setBackground(backgroundColor);

			final var glTitleImage = new GridLayout();
			glTitleImage.marginWidth = 0;
			glTitleImage.verticalSpacing = 0;
			glTitleImage.marginHeight = 0;
			glTitleImage.horizontalSpacing = 0;

			final var panTitleImage = new Composite(panTitleArea, SWT.NONE);
			panTitleImage.setLayout(glTitleImage);
			panTitleImage.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 3));

			final var lblTitleImage = new Label(panTitleImage, SWT.NONE);
			lblTitleImage.setBackground(backgroundColor);
			lblTitleImage.setImage(JFaceResources.getImage(TitleAreaDialog.DLG_IMG_TITLE_BANNER));
			lblTitleImage.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));

			final var gdTitleMessage = new GridData(SWT.LEFT, SWT.CENTER, true, true, 1, 2);
			gdTitleMessage.horizontalIndent = 20;

			final var lblTitleMessage = new Label(panTitleArea, SWT.NONE);
			lblTitleMessage.setLayoutData(gdTitleMessage);
			lblTitleMessage.setFont(JFaceResources.getDialogFont());
			lblTitleMessage.setBackground(backgroundColor);
			lblTitleMessage.setText("Form initialized successfully!");

			final var lblTitleSeparator = new Label(panTitleArea, SWT.SEPARATOR | SWT.HORIZONTAL);
			lblTitleSeparator.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		}

		panPreview = new Composite(panForm, SWT.NONE);
		panPreview.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panPreview.setLayout(new GridLayout());

		if (addButtonBar) {
			final var gdButtons = new GridData(SWT.FILL, SWT.BOTTOM, true, false);
			gdButtons.heightHint = 50;

			final var panButtons = new Composite(panForm, SWT.NONE);
			panButtons.setLayout(new GridLayout(3, false));
			panButtons.setLayoutData(gdButtons);

			final var panButtonFill = new Composite(panButtons, SWT.NONE);
			panButtonFill.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

			final var gdOK = new GridData(SWT.LEFT, SWT.CENTER, false, true);
			gdOK.widthHint = 100;

			final var cmdOK = new Button(panButtons, SWT.NONE);
			cmdOK.setLayoutData(gdOK);
			cmdOK.setText("OK");

			final var gdCancel = new GridData(SWT.LEFT, SWT.CENTER, false, true);
			gdCancel.widthHint = 100;

			final var cmdCanel = new Button(panButtons, SWT.NONE);
			cmdCanel.setLayoutData(gdCancel);
			cmdCanel.setText("Cancel");
		}

		if (form != null)
			previewBuilder = new VisualFormEditorPreviewBuilder(panPreview, eventController);

		if (gridPanel != null)
			previewBuilder = new VisualFormEditorPreviewBuilder(panPreview, eventController);

		previewBuilder.setEditMode(editMode);
		previewBuilder.setInitService(initService);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener#onPreviewChanged()
	 */
	@Override
	public void onPreviewChanged() {
		generatePreview();
	}

	/**
	 * Generate the preview
	 */
	public void generatePreview() {
		try {
			// Dispose all existing controls of the dynamic preview panel
			for (final Control control : panPreview.getChildren())
				control.dispose();

			if (form != null)
				previewBuilder.generateFormPreview(form);

			if (gridPanel != null)
				previewBuilder.generateGridPanelPreview(gridPanel);

			panPreview.layout(true);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Widget#dispose()
	 */
	@Override
	public void dispose() {
		super.dispose();

		backgroundColor.dispose();
	}

}
