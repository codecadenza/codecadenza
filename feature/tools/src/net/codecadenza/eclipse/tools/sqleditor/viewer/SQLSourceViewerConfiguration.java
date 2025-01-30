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

import net.codecadenza.eclipse.tools.util.editor.ColorProvider;
import net.codecadenza.eclipse.tools.util.editor.CommonEditorConstants;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.DefaultTextDoubleClickStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.information.IInformationPresenter;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

/**
 * <p>
 * Configuration of the SQL source viewer
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLSourceViewerConfiguration extends SourceViewerConfiguration {
	private InformationPresenter presenter;
	private final SQLTextTools tools;
	private final IDocumentPartitioner partitioner;
	private final SQLSyntax syntax;
	private final DefaultTextDoubleClickStrategy defaultTextDoubleClickStrategy = new DefaultTextDoubleClickStrategy();

	/**
	 * Create a new SQL source viewer configuration
	 * @param syntax the SQL syntax to be used
	 * @param tools the SQL tools to be used
	 */
	public SQLSourceViewerConfiguration(SQLSyntax syntax, SQLTextTools tools) {
		this.tools = tools;
		this.syntax = syntax;
		this.partitioner = tools.createDocumentPartitioner();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getDoubleClickStrategy(org.eclipse.jface.text.source.
	 * ISourceViewer, java.lang.String)
	 */
	@Override
	public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer, String contentType) {
		return defaultTextDoubleClickStrategy;
	}

	/**
	 * @return the document partitioner
	 */
	public IDocumentPartitioner getDocumentPartitioner() {
		return partitioner;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getPresentationReconciler(org.eclipse.jface.text.source.
	 * ISourceViewer)
	 */
	@Override
	public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
		final var colorProvider = new ColorProvider();
		final var reconciler = new PresentationReconciler();
		var dr = new DefaultDamagerRepairer(tools.getCodeScanner());

		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

		// Rule for SQL comments
		final var multiLineScanner = new RuleBasedScanner();
		multiLineScanner
				.setDefaultReturnToken(new Token(new TextAttribute(colorProvider.getColor(ColorProvider.MULTI_LINE_COMMENT))));

		dr = new DefaultDamagerRepairer(multiLineScanner);

		reconciler.setDamager(dr, CommonEditorConstants.TOKEN_MULTI_LINE_COMMENT);
		reconciler.setRepairer(dr, CommonEditorConstants.TOKEN_MULTI_LINE_COMMENT);

		// Rule for literals
		dr = new DefaultDamagerRepairer(tools.getCodeScanner());

		reconciler.setDamager(dr, CommonEditorConstants.TOKEN_LITERAL);
		reconciler.setRepairer(dr, CommonEditorConstants.TOKEN_LITERAL);

		return reconciler;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getTabWidth(org.eclipse.jface.text.source.ISourceViewer)
	 */
	@Override
	public int getTabWidth(ISourceViewer sourceViewer) {
		return 4;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getContentAssistant(org.eclipse.jface.text.source.ISourceViewer)
	 */
	@Override
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		final var assistant = new ContentAssistant();
		final var processor = new SQLCompletionProcessor(syntax);

		assistant.setContentAssistProcessor(processor, IDocument.DEFAULT_CONTENT_TYPE);

		// Register the same processor for strings and single-line comments to get code completion at the start of those partitions
		assistant.setContentAssistProcessor(processor, CommonEditorConstants.TOKEN_LITERAL);
		assistant.setContentAssistProcessor(processor, CommonEditorConstants.TOKEN_MULTI_LINE_COMMENT);
		assistant.enableAutoActivation(true);
		assistant.setAutoActivationDelay(500);
		assistant.enableAutoInsert(true);
		assistant.enableAutoActivation(true);
		assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
		assistant.setInformationControlCreator(getInformationControlCreator(sourceViewer));

		return assistant;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getInformationPresenter(org.eclipse.jface.text.source.
	 * ISourceViewer)
	 */
	@Override
	public IInformationPresenter getInformationPresenter(ISourceViewer sourceViewer) {
		if (presenter == null) {
			final IInformationControlCreator informationControlCreator = DefaultInformationControl::new;

			presenter = new InformationPresenter(informationControlCreator);
			presenter.setSizeConstraints(60, 10, true, true);
		}

		return presenter;
	}

}
