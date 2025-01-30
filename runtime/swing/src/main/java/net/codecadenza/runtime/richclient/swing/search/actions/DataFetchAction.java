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
package net.codecadenza.runtime.richclient.swing.search.actions;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_MSG_NO_DATA;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_RESULT_WITH_COUNT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATA_FETCH_ACTION_STATUS_FETCH_DATA;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicReference;
import javax.swing.AbstractAction;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.search.panel.AbstractSearchResultDataPanel;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Action to fetch data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data fetch action
 */
public class DataFetchAction<T> extends AbstractAction {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 6840860983935051201L;

	private final AbstractSearchResultDataPanel<T> panel;
	private final AtomicReference<Thread> worker = new AtomicReference<>();

	/**
	 * Constructor
	 * @param panel
	 */
	public DataFetchAction(AbstractSearchResultDataPanel<T> panel) {
		super(getTranslation(DATA_FETCH_ACTION_NAME), ImageLoader.getImage(ImageLoader.REFRESH));

		this.panel = panel;

		putValue(SHORT_DESCRIPTION, getTranslation(DATA_FETCH_ACTION_SHORT_DESC));
	}

	private final class Fetcher implements Runnable {
		/*
		 * (non-Javadoc)
		 * @see java.lang.Runnable#run()
		 */
		@Override
		public void run() {
			logger.debug("Perform data fetch operation");

			try {
				final SearchDTO searchObj = panel.getSearchObj();
				searchObj.setFetchHidden(true);
				searchObj.setStartIndex((panel.getPageIndex() - 1) * searchObj.getMaxResult());

				final long t0 = System.currentTimeMillis();
				final Collection<T> values = panel.fetchData();
				final long diff = System.currentTimeMillis() - t0;

				if (values == null)
					throw new IllegalStateException(getTranslation(DATA_FETCH_ACTION_MSG_NO_DATA));

				long totalCount = 0;

				if (searchObj.isCount())
					totalCount = panel.countData(searchObj);

				// Don't stop us past that point, we have our data anyway
				worker.set(null);

				var message = "";
				final boolean enableNext;
				final boolean enablePrev = panel.getPageIndex() > 1;

				if (searchObj.isCount()) {
					final int pageLen = searchObj.getMaxResult();
					final long pages = (totalCount + pageLen - 1) / pageLen;
					enableNext = panel.getPageIndex() < pages;

					final var params = new ArrayList<>();
					params.add(values.size());
					params.add(totalCount);
					params.add(String.format("%.2f", (double) diff / 1000));
					params.add(pages > 0 ? panel.getPageIndex() : 0);
					params.add(pages);

					message = getTranslation(DATA_FETCH_ACTION_RESULT_WITH_COUNT, params.toArray());
				}
				else {
					final var params = new ArrayList<>();
					params.add(values.size());
					params.add(String.format("%.2f", (double) diff / 1000));

					message = getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, params.toArray());

					enableNext = false;
				}

				final String queryInfo = message;

				EventQueue.invokeLater(() -> {
					resetView(enableNext, enablePrev);

					panel.setStatusInfoMessage(queryInfo);
					panel.getTable().setData(values);
					panel.setBusy(false);

					if (!panel.isUserDefQuery())
						SearchManager.saveLastSearch(panel.getViewID(), panel.getSearchObj());

					logger.debug("Data fetch operation finished");
				});
			}
			catch (final Exception e) {
				worker.set(null);

				logger.error("Error while fetching data!", e);

				EventQueue.invokeLater(() -> {
					resetView(false, false);
					panel.setStatusErrorMessage(getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED) + e.getMessage());
				});
			}
		}

		/**
		 * @param next
		 * @param prev
		 */
		private void resetView(boolean next, boolean prev) {
			panel.setBusy(false);
			panel.getNextPageAction().setEnabled(next);
			panel.getPrevPageAction().setEnabled(prev);
		}
	}

	/**
	 * Stop execution
	 */
	@SuppressWarnings("removal")
	public void abort() {
		final Thread thisWorker = this.worker.getAndSet(null);

		if (thisWorker != null)
			thisWorker.stop();
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void actionPerformed(ActionEvent arg0) {
		if (this.worker.get() != null)
			throw new IllegalStateException();

		panel.setStatusInfoMessage(getTranslation(DATA_FETCH_ACTION_STATUS_FETCH_DATA));
		panel.getTable().setData((Collection<T>) Collections.emptyList());
		panel.setBusy(true);

		final var thisWorker = new Thread(new Fetcher());
		this.worker.set(thisWorker);

		thisWorker.setDaemon(true);
		thisWorker.start();
	}

}
