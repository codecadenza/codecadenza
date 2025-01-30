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
package net.codecadenza.eclipse.tools.jpaeditor.viewer;

import net.codecadenza.eclipse.tools.util.editor.CommonEditorConstants;
import net.codecadenza.eclipse.tools.util.editor.PartitionScanner;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.RuleBasedScanner;

/**
 * <p>
 * Tools that are required to configure a JFace text viewer
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPATextTools {
	private JPACodeScanner codeScanner;
	private PartitionScanner partitionScanner;

	/**
	 * Constructor
	 * @param syntax
	 */
	public JPATextTools(JPASyntax syntax) {
		codeScanner = new JPACodeScanner(syntax);
		partitionScanner = new PartitionScanner();
	}

	/**
	 * Dispose all internal resources
	 */
	public void dispose() {
		codeScanner = null;
		partitionScanner = null;
	}

	/**
	 * @return the code scanner
	 */
	public RuleBasedScanner getCodeScanner() {
		return codeScanner;
	}

	/**
	 * @return the partition scanner
	 */
	public IPartitionTokenScanner getPartitionScanner() {
		return partitionScanner;
	}

	/**
	 * Create a document partitioner
	 * @return the document partitioner
	 */
	public IDocumentPartitioner createDocumentPartitioner() {
		final var types = new String[] { IDocument.DEFAULT_CONTENT_TYPE, CommonEditorConstants.TOKEN_LITERAL,
				CommonEditorConstants.TOKEN_MULTI_LINE_COMMENT, CommonEditorConstants.TOKEN_SINGEL_LINE_COMMENT };

		return new FastPartitioner(getPartitionScanner(), types);
	}

}
