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
package net.codecadenza.eclipse.tools.sqleditor.viewer;

import net.codecadenza.eclipse.tools.util.editor.CommonEditorConstants;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextViewerUndoManager;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.VerticalRuler;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Implementation of the SQL text viewer widget
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLTextViewer extends SourceViewer {
	private IPresentationReconciler presentationReconciler;
	private IDocumentPartitioner partitioner;
	private SQLTextTools sqlTextTools;
	private IContentAssistant contentAssistant;
	private SQLSourceViewerConfiguration configuration;
	private final TextViewerUndoManager undoManager = new TextViewerUndoManager(50);

	/**
	 * Constructor of SQL text viewer
	 * @param parent
	 * @param style
	 * @param syntax
	 */
	public SQLTextViewer(Composite parent, int style, SQLSyntax syntax) {
		this(parent, style, syntax, new VerticalRuler(0));
	}

	/**
	 * Constructor of SQL text viewer
	 * @param parent
	 * @param style
	 * @param syntax
	 * @param ruler
	 */
	public SQLTextViewer(Composite parent, int style, SQLSyntax syntax, IVerticalRuler ruler) {
		super(parent, ruler, style);

		sqlTextTools = new SQLTextTools(syntax);

		this.getControl().addDisposeListener(_ -> {
			sqlTextTools.dispose();
			presentationReconciler.uninstall();
		});

		configuration = new SQLSourceViewerConfiguration(syntax, sqlTextTools);
		presentationReconciler = configuration.getPresentationReconciler(null);
		contentAssistant = configuration.getContentAssistant(null);
		contentAssistant.install(this);
		presentationReconciler.install(this);
		partitioner = configuration.getDocumentPartitioner();

		parent.addDisposeListener(_ -> {
			if (contentAssistant != null) {
				try {
					contentAssistant.uninstall();
				}
				catch (final Exception _) {
					// This exception will be ignored!
				}
			}
		});

		fInformationPresenter = configuration.getInformationPresenter(this);
		fInformationPresenter.install(this);

		final String[] contentTypes = { IDocument.DEFAULT_CONTENT_TYPE, CommonEditorConstants.TOKEN_LITERAL,
				CommonEditorConstants.TOKEN_MULTI_LINE_COMMENT, CommonEditorConstants.TOKEN_SINGEL_LINE_COMMENT };

		for (final String contentType : contentTypes) {
			super.setTextHover(new ITextHover() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.jface.text.ITextHover#getHoverInfo(org.eclipse.jface.text.ITextViewer, org.eclipse.jface.text.IRegion)
				 */
				@Override
				public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
					return "";
				}

				/*
				 * (non-Javadoc)
				 * @see org.eclipse.jface.text.ITextHover#getHoverRegion(org.eclipse.jface.text.ITextViewer, int)
				 */
				@Override
				public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
					return new Region(offset, 1);
				}

			}, contentType);
		}

		super.activatePlugins();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewer#setDocument(org.eclipse.jface.text.IDocument)
	 */
	@Override
	public void setDocument(IDocument dc) {
		final IDocument previous = this.getDocument();

		if (previous != null)
			partitioner.disconnect();

		super.setDocument(dc);

		if (dc != null) {
			partitioner.connect(dc);
			dc.setDocumentPartitioner(partitioner);
			undoManager.connect(this);
			this.setUndoManager(undoManager);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewer#setDocument(org.eclipse.jface.text.IDocument,
	 * org.eclipse.jface.text.source.IAnnotationModel)
	 */
	@Override
	public void setDocument(IDocument document, IAnnotationModel annotationModel) {
		setDocument(document);

		if (annotationModel != null && document != null)
			annotationModel.connect(document);
	}

	/**
	 * Refresh the viewer
	 * @param syntax
	 */
	public void refresh(SQLSyntax syntax) {
		if (contentAssistant != null) {
			contentAssistant.uninstall();
			contentAssistant = null;
		}

		sqlTextTools = new SQLTextTools(syntax);
		configuration = new SQLSourceViewerConfiguration(syntax, sqlTextTools);

		fPresentationReconciler = configuration.getPresentationReconciler(null);
		fPresentationReconciler.install(this);

		contentAssistant = configuration.getContentAssistant(null);
		contentAssistant.install(this);
	}

	/**
	 * Show the possible completions
	 */
	public void showAssistance() {
		contentAssistant.showPossibleCompletions();
	}

	/**
	 * Clear the text
	 */
	public void clearText() {
		getTextWidget().setText("");
	}

}
