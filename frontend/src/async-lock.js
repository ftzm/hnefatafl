/**
 * A FIFO queue-based async lock for preventing race conditions
 */
export class AsyncLock {
	constructor() {
		this._lockQueue = [];
		this._isLocked = false;
	}

	async acquire() {
		return new Promise(resolve => {
			if (!this._isLocked) {
				this._isLocked = true;
				resolve();
			} else {
				this._lockQueue.push(resolve);
			}
		});
	}

	release() {
		if (this._lockQueue.length > 0) {
			const [nextResolve] = this._lockQueue.splice(0, 1);
			nextResolve();
		} else {
			this._isLocked = false;
		}
	}

	async withLock(fn) {
		await this.acquire();
		try {
			return await fn();
		} finally {
			this.release();
		}
	}
}
